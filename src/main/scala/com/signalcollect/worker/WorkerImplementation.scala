/*
 *  @author Philip Stutz
 *  @author Mihaela Verman
 *
 *  Copyright 2012 University of Zurich
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package com.signalcollect.worker

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.lang.management.ManagementFactory
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.annotation.tailrec
import scala.util.Random
import scala.collection.mutable.Queue
import com.signalcollect.Edge
import com.signalcollect.GraphEditor
import com.signalcollect.Vertex
import com.signalcollect.interfaces.ComplexAggregation
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandler
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandlerFactory
import com.signalcollect.interfaces.EdgeId
import com.signalcollect.interfaces.ExistingVertexHandler
import com.signalcollect.interfaces.ExistingVertexHandlerFactory
import com.signalcollect.interfaces.MessageBus
import com.signalcollect.interfaces.NodeStatistics
import com.signalcollect.interfaces.Scheduler
import com.signalcollect.interfaces.SchedulerFactory
import com.signalcollect.interfaces.SentMessagesStats
import com.signalcollect.interfaces.Storage
import com.signalcollect.interfaces.StorageFactory
import com.signalcollect.interfaces.UndeliverableSignalHandler
import com.signalcollect.interfaces.UndeliverableSignalHandlerFactory
import com.signalcollect.interfaces.Worker
import com.signalcollect.interfaces.WorkerApi
import com.signalcollect.interfaces.WorkerStatistics
import com.signalcollect.interfaces.WorkerStatus
import com.signalcollect.serialization.DefaultSerializer
import com.sun.management.OperatingSystemMXBean
import akka.actor.ActorRef
import akka.event.LoggingAdapter
import com.signalcollect.util.IteratorConcatenator

/**
 * Main implementation of the WorkerApi interface.
 */
class WorkerImplementation[@specialized(Int, Long) Id, Signal](
  val workerId: Int,
  val numberOfWorkers: Int,
  val numberOfNodes: Int,
  val isEagerIdleDetectionEnabled: Boolean,
  val isThrottlingEnabled: Boolean,
  val supportBlockingGraphModificationsInVertex: Boolean,
  val messageBus: MessageBus[Id, Signal],
  val log: LoggingAdapter,
  val storageFactory: StorageFactory[Id, Signal],
  val schedulerFactory: SchedulerFactory[Id, Signal],
  val existingVertexHandlerFactory: ExistingVertexHandlerFactory[Id, Signal],
  val undeliverableSignalHandlerFactory: UndeliverableSignalHandlerFactory[Id, Signal],
  val edgeAddedToNonExistentVertexHandlerFactory: EdgeAddedToNonExistentVertexHandlerFactory[Id, Signal],
  var signalThreshold: Double,
  var collectThreshold: Double)
  extends Worker[Id, Signal] {

  val pendingModifications = new IteratorConcatenator[GraphEditor[Id, Signal] => Unit]()
  val workersPerNode = numberOfWorkers / numberOfNodes // Assumes that there is the same number of workers on all nodes.
  val nodeId = getNodeId(workerId)
  val pingPongSchedulingIntervalInMilliseconds = 4 // schedule pingpong exchange every 8ms
  val maxPongDelay = 4e+6 // pong is considered delayed after waiting for 4ms  
  var scheduler: Scheduler[Id, Signal] = _
  var graphEditor: GraphEditor[Id, Signal] = _

  initialize

  var messageBusFlushed: Boolean = _
  var isIdleDetectionEnabled: Boolean = _
  var slowPongDetected: Boolean = _ // If the worker had to wait too long for the last pong reply to its ping request.
  var operationsScheduled: Boolean = _ // If executing operations has been scheduled.
  var isIdle: Boolean = _ // Idle status that was last reported to the coordinator.
  var isPaused: Boolean = _
  var allWorkDoneWhenContinueSent: Boolean = _
  var lastStatusUpdate: Long = _
  var vertexStore: Storage[Id, Signal] = _
  var pingSentTimestamp: Long = _
  var pingPongScheduled: Boolean = _
  var waitingForPong: Boolean = _
  var existingVertexHandler: ExistingVertexHandler[Id, Signal] = _
  var undeliverableSignalHandler: UndeliverableSignalHandler[Id, Signal] = _
  var edgeAddedToNonExistentVertexHandler: EdgeAddedToNonExistentVertexHandler[Id, Signal] = _
  isIdleDetectionEnabled = false // This one should not be reset.
  operationsScheduled = false // This one should not be reset.

  val counters: WorkerOperationCounters = new WorkerOperationCounters()

  def initialize {
    messageBusFlushed = true
    slowPongDetected = false
    isIdle = true
    isPaused = true
    allWorkDoneWhenContinueSent = false
    lastStatusUpdate = System.currentTimeMillis
    vertexStore = storageFactory.createInstance
    pendingModifications.clear
    pingSentTimestamp = 0
    pingPongScheduled = false
    waitingForPong = false
    scheduler = schedulerFactory.createInstance(this)
    graphEditor = if (supportBlockingGraphModificationsInVertex) {
      new WorkerGraphEditor[Id, Signal](workerId, this, messageBus)
    } else {
      messageBus.getGraphEditor
    }
    existingVertexHandler = existingVertexHandlerFactory.createInstance
    undeliverableSignalHandler = undeliverableSignalHandlerFactory.createInstance
    edgeAddedToNonExistentVertexHandler = edgeAddedToNonExistentVertexHandlerFactory.createInstance
  }

  def getNodeId(workerId: Int): Int = workerId / workersPerNode
  def getRandomPingPongPartner = Random.nextInt(numberOfWorkers)

  def sendPing(partner: Int) {
    if (messageBus.isInitialized) {
      pingPongScheduled = true
      waitingForPong = true
      pingSentTimestamp = System.nanoTime
      messageBus.sendToWorkerUncounted(partner, Ping(workerId))
    }
  }

  /**
   * Resets all state apart from that which is part of the constructor.
   * Also does not reset the part of the counters which is part of
   * termination detection.
   */
  override def reset {
    initialize
    counters.resetOperationCounters
    messageBus.reset
  }

  def isAllWorkDone: Boolean = {
    if (isPaused) {
      pendingModifications.isEmpty
    } else {
      isConverged
    }
  }

  override def initializeIdleDetection {
    isIdleDetectionEnabled = true

    // Ensure that the current status is immediately reported.
    if (isEagerIdleDetectionEnabled) {
      messageBus.sendToNodeUncounted(nodeId, getWorkerStatusForNode)
    } else {
      sendStatusToCoordinator
    }

    // Initiate PingPong throttling.
    if (isThrottlingEnabled) {
      if (numberOfNodes > 1) {
        // Sent to a random worker on the next node initially.
        val partnerNodeId = (nodeId + 1) % (numberOfNodes - 1)
        val workerOnNode = Random.nextInt(workersPerNode)
        val workerId = partnerNodeId * workersPerNode + workerOnNode
        sendPing(workerId)
      } else {
        sendPing(getRandomPingPongPartner)
      }
    }

  }

  def sendStatusToCoordinator {
    if (messageBus.isInitialized) {
      val status = getWorkerStatusForCoordinator
      messageBus.sendToCoordinatorUncounted(status)
    }
  }

  def isConverged: Boolean = {
    vertexStore.toCollect.isEmpty &&
      vertexStore.toSignal.isEmpty &&
      messageBusFlushed
  }

  def executeCollectOperationOfVertex(vertex: Vertex[Id, _, Id, Signal], addToSignal: Boolean = true) {
    counters.collectOperationsExecuted += 1
    vertex.executeCollectOperation(graphEditor)
    if (addToSignal && vertex.scoreSignal > signalThreshold) {
      vertexStore.toSignal.put(vertex)
    }
  }

  def executeSignalOperationOfVertex(vertex: Vertex[Id, _, Id, Signal]) {
    counters.signalOperationsExecuted += 1
    vertex.executeSignalOperation(graphEditor)
  }

  def processBulkSignalWithoutIds(signals: Array[Signal], targetIds: Array[Id]) {
    val size = signals.length
    var i = 0
    while (i < size) {
      processSignalWithoutSourceId(signals(i), targetIds(i))
      i += 1
    }
  }

  override def processSignalWithSourceId(signal: Signal, targetId: Id, sourceId: Id) {
    val vertex = vertexStore.vertices.get(targetId)
    if (vertex != null) {
      if (vertex.deliverSignalWithSourceId(signal, sourceId, graphEditor)) {
        counters.collectOperationsExecuted += 1
        if (vertex.scoreSignal > signalThreshold) {
          scheduler.handleCollectOnDelivery(vertex)
        }
      } else {
        if (vertex.scoreCollect > collectThreshold) {
          vertexStore.toCollect.put(vertex)
        }
      }
    } else {
      undeliverableSignalHandler.vertexForSignalNotFound(signal, targetId, Some(sourceId), graphEditor)
    }
    messageBusFlushed = false
  }

  override def processSignalWithoutSourceId(signal: Signal, targetId: Id) {
    val vertex = vertexStore.vertices.get(targetId)
    if (vertex != null) {
      if (vertex.deliverSignalWithoutSourceId(signal, graphEditor)) {
        counters.collectOperationsExecuted += 1
        if (vertex.scoreSignal > signalThreshold) {
          scheduler.handleCollectOnDelivery(vertex)
        }
      } else {
        if (vertex.scoreCollect > collectThreshold) {
          vertexStore.toCollect.put(vertex)
        }
      }
    } else {
      undeliverableSignalHandler.vertexForSignalNotFound(signal, targetId, None, graphEditor)
    }
    messageBusFlushed = false
  }

  override def startComputation {
    if (!pendingModifications.isEmpty) {
      log.warning("Need to call `awaitIdle` after executiong `loadGraph` or pending operations are ignored.")
    }
    isPaused = false
  }

  override def pauseComputation {
    isPaused = true
  }

  override def signalStep: Boolean = {
    counters.signalSteps += 1
    vertexStore.toSignal.process(executeSignalOperationOfVertex(_))
    messageBus.flush
    messageBusFlushed = true
    true // always returns true, just to make it blocking.
  }

  override def collectStep: Boolean = {
    counters.collectSteps += 1
    vertexStore.toCollect.process(executeCollectOperationOfVertex(_))
    vertexStore.toSignal.isEmpty
  }

  override def addVertex(vertex: Vertex[Id, _, Id, Signal]) {
    if (vertexStore.vertices.put(vertex)) {
      counters.verticesAdded += 1
      counters.outgoingEdgesAdded += vertex.edgeCount
      vertex.afterInitialization(graphEditor)
      messageBusFlushed = false
      if (vertex.scoreSignal > signalThreshold) {
        vertexStore.toSignal.put(vertex)
      }
    } else {
      val existing = vertexStore.vertices.get(vertex.id)
      existingVertexHandler.mergeVertices(existing, vertex, graphEditor)
    }
  }

  override def addEdge(sourceId: Id, edge: Edge[Id]) {
    def addEdgeToVertex(vertex: Vertex[Id, _, Id, Signal]) {
      if (vertex.addEdge(edge, graphEditor)) {
        counters.outgoingEdgesAdded += 1
        if (vertex.scoreSignal > signalThreshold) {
          vertexStore.toSignal.put(vertex)
        }
      }
    }
    val v = vertexStore.vertices.get(sourceId)
    if (v == null) {
      val vertexOption = edgeAddedToNonExistentVertexHandler.handleImpossibleEdgeAddition(edge, sourceId)
      if (vertexOption.isDefined) {
        addVertex(vertexOption.get)
        addEdgeToVertex(vertexOption.get)
      }
    } else {
      addEdgeToVertex(v)
    }
  }

  override def removeEdge(edgeId: EdgeId[Id]) {
    val vertex = vertexStore.vertices.get(edgeId.sourceId)
    if (vertex != null) {
      if (vertex.removeEdge(edgeId.targetId, graphEditor)) {
        counters.outgoingEdgesRemoved += 1
        if (vertex.scoreSignal > signalThreshold) {
          vertexStore.toSignal.put(vertex)
        }
      } else {
        log.warning("Outgoing edge not found when trying to remove edge with id " + edgeId)
      }
    } else {
      log.warning("Source vertex not found found when trying to remove outgoing edge with id " + edgeId)
    }
  }

  override def removeVertex(vertexId: Id) {
    val vertex = vertexStore.vertices.get(vertexId)
    if (vertex != null) {
      processRemoveVertex(vertex)
    } else {
      log.warning("Should remove vertex with id " + vertexId + ": could not find this vertex.")
    }
  }

  protected def processRemoveVertex(vertex: Vertex[Id, _, Id, Signal]) {
    val edgesRemoved = vertex.removeAllEdges(graphEditor)
    counters.outgoingEdgesRemoved += edgesRemoved
    counters.verticesRemoved += 1
    vertex.beforeRemoval(graphEditor)
    vertexStore.vertices.remove(vertex.id)
    vertexStore.toCollect.remove(vertex.id)
    vertexStore.toSignal.remove(vertex.id)
  }

  override def modifyGraph(graphModification: GraphEditor[Id, Signal] => Unit, vertexIdHint: Option[Id]) {
    graphModification(graphEditor)
    messageBusFlushed = false
  }

  override def loadGraph(graphModifications: Iterator[GraphEditor[Id, Signal] => Unit], vertexIdHint: Option[Id]) {
    pendingModifications.appendIterator(graphModifications) // To avoid https://issues.scala-lang.org/browse/SI-8428, which is not really fixed.
  }

  override def setSignalThreshold(st: Double) {
    signalThreshold = st
  }

  override def setCollectThreshold(ct: Double) {
    collectThreshold = ct
  }

  override def recalculateScores {
    vertexStore.vertices.foreach(recalculateVertexScores(_))
  }

  override def recalculateScoresForVertexWithId(vertexId: Id) {
    val vertex = vertexStore.vertices.get(vertexId)
    if (vertex != null) {
      recalculateVertexScores(vertex)
    }
  }

  protected def recalculateVertexScores(vertex: Vertex[Id, _, Id, Signal]) {
    if (vertex.scoreCollect > collectThreshold) {
      vertexStore.toCollect.put(vertex)
    }
    if (vertex.scoreSignal > signalThreshold) {
      vertexStore.toSignal.put(vertex)
    }
  }

  override def forVertexWithId[VertexType <: Vertex[Id, _, Id, Signal], ResultType](vertexId: Id, f: VertexType => ResultType): ResultType = {
    val vertex = vertexStore.vertices.get(vertexId)
    if (vertex != null) {
      val result = f(vertex.asInstanceOf[VertexType])
      result
    } else {
      throw new Exception("Vertex with id " + vertexId + " not found.")
    }
  }

  override def foreachVertex(f: Vertex[Id, _, Id, Signal] => Unit) {
    vertexStore.vertices.foreach(f)
  }

  override def foreachVertexWithGraphEditor(f: GraphEditor[Id, Signal] => Vertex[Id, _, Id, Signal] => Unit) {
    val function = f(graphEditor)
    vertexStore.vertices.foreach(function)
    messageBusFlushed = false
  }

  override def aggregateOnWorker[WorkerResult](aggregationOperation: Array[Byte]): Array[Byte] = {
    DefaultSerializer.write(aggregateOnWorker(DefaultSerializer.read[ComplexAggregation[WorkerResult, _]](aggregationOperation)))
  }

  override def aggregateOnWorker[WorkerResult](aggregationOperation: ComplexAggregation[WorkerResult, _]): WorkerResult = {
    aggregationOperation.aggregationOnWorker(vertexStore.vertices.stream)
  }

  override def aggregateAll[WorkerResult, EndResult](aggregationOperation: ComplexAggregation[WorkerResult, EndResult]): EndResult = {
    throw new UnsupportedOperationException("AkkaWorker does not support this operation.")
  }

  /**
   * Creates a snapshot of all the vertices in all workers.
   * Does not store the toSignal/toCollect collections or pending messages.
   * Should only be used when the workers are idle.
   * Overwrites any previous snapshot that might exist.
   */
  override def snapshot {
    // Overwrites previous file if it should exist.
    val snapshotFileOutput = new DataOutputStream(new FileOutputStream(s"$workerId.snapshot"))
    try {
      vertexStore.vertices.foreach { vertex =>
        val bytes = DefaultSerializer.write(vertex)
        snapshotFileOutput.writeInt(bytes.length)
        snapshotFileOutput.write(bytes)
      }
    } catch {
      case t: Throwable =>
        val msg = s"Problem while serializing a vertex, this will prevent 'restore' from working correctly: ${t.getMessage}"
        println(msg)
        t.printStackTrace
        log.error(t, msg)
    } finally {
      snapshotFileOutput.close
    }
  }

  /**
   * Restores the last snapshot of all the vertices in all workers.
   * Does not store the toSignal/toCollect collections or pending messages.
   * Should only be used when the workers are idle.
   */
  override def restore {
    reset
    val maxSerializedSize = 64768
    val snapshotFile = new File(s"$workerId.snapshot")
    val buffer = new Array[Byte](maxSerializedSize)
    if (snapshotFile.exists) {
      val snapshotFileInput = new DataInputStream(new FileInputStream(snapshotFile))
      val buffer = new Array[Byte](maxSerializedSize)
      while (snapshotFileInput.available > 0) {
        val serializedLength = snapshotFileInput.readInt
        assert(serializedLength <= maxSerializedSize)
        val bytesRead = snapshotFileInput.read(buffer, 0, serializedLength)
        assert(bytesRead == serializedLength)
        val vertex = DefaultSerializer.read[Vertex[Id, _, Id, Signal]](buffer)
        addVertex(vertex)
      }
      snapshotFileInput.close
    }
  }

  /**
   * Deletes the worker snapshots if they exist.
   */
  override def deleteSnapshot {
    val snapshotFile = new File(s"$workerId.snapshot")
    if (snapshotFile.exists) {
      snapshotFile.delete
    }
  }

  def getWorkerStatusForCoordinator: WorkerStatus = {
    WorkerStatus(
      workerId = workerId,
      timeStamp = System.nanoTime,
      isIdle = isIdle,
      isPaused = isPaused,
      messagesSent = SentMessagesStats(
        messageBus.messagesSentToWorkers,
        messageBus.messagesSentToNodes,
        messageBus.messagesSentToCoordinator,
        messageBus.messagesSentToOthers),
      messagesReceived = counters.messagesReceived)
  }

  def getWorkerStatusForNode: WorkerStatus = {
    val ws = WorkerStatus(
      workerId = workerId,
      timeStamp = System.nanoTime,
      isIdle = isIdle,
      isPaused = isPaused,
      messagesSent = SentMessagesStats(
        messageBus.messagesSentToWorkers,
        messageBus.messagesSentToNodes,
        messageBus.messagesSentToCoordinator,
        messageBus.messagesSentToOthers),
      messagesReceived = counters.messagesReceived)
    ws.messagesSent.nodes(nodeId) = ws.messagesSent.nodes(nodeId)
    ws
  }

  override def getIndividualWorkerStatistics: List[WorkerStatistics] = List(getWorkerStatistics)

  override def getWorkerStatistics: WorkerStatistics = {
    WorkerStatistics(
      workerId = Some(workerId),
      toSignalSize = vertexStore.toSignal.size,
      toCollectSize = vertexStore.toCollect.size,
      collectOperationsExecuted = counters.collectOperationsExecuted,
      signalOperationsExecuted = counters.signalOperationsExecuted,
      numberOfVertices = vertexStore.vertices.size,
      verticesAdded = counters.verticesAdded,
      verticesRemoved = counters.verticesRemoved,
      numberOfOutgoingEdges = counters.outgoingEdgesAdded - counters.outgoingEdgesRemoved, //only valid if no edges are removed during execution
      outgoingEdgesAdded = counters.outgoingEdgesAdded,
      outgoingEdgesRemoved = counters.outgoingEdgesRemoved,
      signalMessagesReceived = counters.signalMessagesReceived,
      bulkSignalMessagesReceived = counters.bulkSignalMessagesReceived,
      continueMessagesReceived = counters.continueMessagesReceived,
      requestMessagesReceived = counters.requestMessagesReceived,
      otherMessagesReceived = counters.otherMessagesReceived,
      messagesSentToWorkers = messageBus.messagesSentToWorkers.sum,
      messagesSentToNodes = messageBus.messagesSentToNodes.sum,
      messagesSentToCoordinator = messageBus.messagesSentToCoordinator,
      messagesSentToOthers = messageBus.messagesSentToOthers)
  }

  // TODO: Move this method to Node and use proper node id.
  override def getIndividualNodeStatistics: List[NodeStatistics] = List(getNodeStatistics)

  // TODO: Move this method to Node and use proper node id.
  override def getNodeStatistics: NodeStatistics = {
    val runtime: Runtime = Runtime.getRuntime
    try {
      val osBean: OperatingSystemMXBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      NodeStatistics(
        nodeId = Some(workerId),
        os = System.getProperty("os.name"),
        runtime_mem_total = runtime.totalMemory,
        runtime_mem_max = runtime.maxMemory,
        runtime_mem_free = runtime.freeMemory,
        runtime_cores = runtime.availableProcessors,
        jmx_committed_vms = osBean.getCommittedVirtualMemorySize,
        jmx_mem_free = osBean.getFreePhysicalMemorySize,
        jmx_mem_total = osBean.getTotalPhysicalMemorySize,
        jmx_swap_free = osBean.getFreeSwapSpaceSize,
        jmx_swap_total = osBean.getTotalSwapSpaceSize,
        jmx_process_load = osBean.getProcessCpuLoad,
        jmx_process_time = osBean.getProcessCpuTime,
        jmx_system_load = osBean.getSystemCpuLoad)
    } catch {
      case notSupported: NoSuchMethodError =>
        NodeStatistics(
          nodeId = Some(workerId),
          os = System.getProperty("os.name"),
          runtime_mem_total = runtime.totalMemory,
          runtime_mem_max = runtime.maxMemory,
          runtime_mem_free = runtime.freeMemory,
          runtime_cores = runtime.availableProcessors)
    }
  }

  protected def logIntialization {
    if (messageBus.isInitialized) {
      val msg = s"Worker $workerId has a fully initialized message bus."
      log.debug(msg)
    }
  }

  def registerWorker(otherWorkerId: Int, worker: ActorRef) {
    counters.requestMessagesReceived -= 1 // Registration messages are not counted.
    messageBus.registerWorker(otherWorkerId, worker)
    logIntialization
  }

  def registerNode(nodeId: Int, node: ActorRef) {
    counters.requestMessagesReceived -= 1 // Registration messages are not counted.
    messageBus.registerNode(nodeId, node)
    logIntialization
  }

  def registerCoordinator(coordinator: ActorRef) {
    counters.requestMessagesReceived -= 1 // Registration messages are not counted.
    messageBus.registerCoordinator(coordinator)
    logIntialization
  }

}

trait WorkerInterceptor[Id, Signal] extends WorkerApi[Id, Signal] {
  abstract override def addVertex(vertex: Vertex[Id, _, Id, Signal]) = {
    println("addVertex")
    super.addVertex(vertex)
  }
  abstract override def addEdge(sourceId: Id, edge: Edge[Id]) = {
    println("addEdge")
    super.addEdge(sourceId, edge)
  }
  abstract override def removeVertex(vertexId: Id) = {
    println("removeVertex")
    super.removeVertex(vertexId)
  }
  abstract override def removeEdge(edgeId: EdgeId[Id]) = {
    println("removeEdge")
    super.removeEdge(edgeId)
  }
  abstract override def processSignalWithSourceId(signal: Signal, targetId: Id, sourceId: Id) = {
    println("processSignalWithSourceId")
    super.processSignalWithSourceId(signal, targetId, sourceId)
  }
  abstract override def processSignalWithoutSourceId(signal: Signal, targetId: Id) = {
    println("processSignalWithoutSourceId")
    super.processSignalWithoutSourceId(signal, targetId)
  }
  abstract override def modifyGraph(graphModification: GraphEditor[Id, Signal] => Unit, vertexIdHint: Option[Id] = None) = {
    println("modifyGraph")
    super.modifyGraph(graphModification, vertexIdHint)
  }
  abstract override def loadGraph(graphModifications: Iterator[GraphEditor[Id, Signal] => Unit], vertexIdHint: Option[Id] = None) = {
    println("loadGraph")
    super.loadGraph(graphModifications, vertexIdHint)
  }
  abstract override def setSignalThreshold(signalThreshold: Double) = {
    println("setSignalThreshold")
    super.setSignalThreshold(signalThreshold)
  }
  abstract override def setCollectThreshold(collectThreshold: Double) = {
    println("setCollectThreshold")
    super.setCollectThreshold(collectThreshold)
  }
  abstract override def recalculateScores = {
    println("recalculateScores")
    super.recalculateScores
  }
  abstract override def recalculateScoresForVertexWithId(vertexId: Id) = {
    println("recalculateScoresForVertexWithId")
    super.recalculateScoresForVertexWithId(vertexId)
  }
  abstract override def forVertexWithId[VertexType <: Vertex[Id, _, Id, Signal], ResultType](vertexId: Id, f: VertexType => ResultType): ResultType = {
    println("forVertexWithId")
    super.forVertexWithId(vertexId, f)
  }
  abstract override def foreachVertex(f: Vertex[Id, _, Id, Signal] => Unit) = {
    println("foreachVertex")
    super.foreachVertex(f)
  }
  abstract override def foreachVertexWithGraphEditor(f: GraphEditor[Id, Signal] => Vertex[Id, _, Id, Signal] => Unit) {
    println("foreachVertexWithGraphEditor")
    super.foreachVertexWithGraphEditor(f)
  }
  abstract override def aggregateOnWorker[WorkerResult](aggregationOperation: ComplexAggregation[WorkerResult, _]): WorkerResult = {
    println("aggregateOnWorker")
    super.aggregateOnWorker(aggregationOperation)
  }
  abstract override def aggregateAll[WorkerResult, EndResult](aggregationOperation: ComplexAggregation[WorkerResult, EndResult]): EndResult = {
    println("aggregateAll")
    super.aggregateAll(aggregationOperation)
  }
  abstract override def pauseComputation = {
    println("pauseComputation")
    super.pauseComputation
  }
  abstract override def startComputation = {
    println("startComputation")
    super.startComputation
  }
  abstract override def signalStep: Boolean = {
    println("signalStep")
    super.signalStep
  }
  abstract override def collectStep: Boolean = {
    println("collectStep")
    super.collectStep
  }
  abstract override def getWorkerStatistics: WorkerStatistics = {
    println("getWorkerStatistics")
    super.getWorkerStatistics
  }
  abstract override def getIndividualWorkerStatistics: List[WorkerStatistics] = {
    println("getIndividualWorkerStatistics")
    super.getIndividualWorkerStatistics
  }
  abstract override def reset = {
    println("reset")
    super.reset
  }
  abstract override def initializeIdleDetection = {
    println("initializeIdleDetection")
    super.initializeIdleDetection
  }
  abstract override def getNodeStatistics: NodeStatistics = {
    println("getNodeStatistics")
    super.getNodeStatistics
  }
  abstract override def getIndividualNodeStatistics: List[NodeStatistics] = {
    println("getIndividualNodeStatistics")
    super.getIndividualNodeStatistics
  }
  abstract override def snapshot = {
    println("snapshot")
    super.snapshot
  }
  abstract override def restore = {
    println("restore")
    super.restore
  }
  abstract override def deleteSnapshot = {
    println("deleteSnapshot")
    super.deleteSnapshot
  }
}
