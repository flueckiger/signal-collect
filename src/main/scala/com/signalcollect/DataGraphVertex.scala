/*
 *  @author Philip Stutz
 *
 *  Copyright 2010 University of Zurich
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

package com.signalcollect

import scala.collection.JavaConversions.mapAsScalaMap

/**
 *  Vertex implementation that collects the most recent signals that have arrived on all edges.
 *  Users of the framework extend this class to implement a specific algorithm by defining a `collect` function.
 *
 *  @note The `collect` function receives as many signals as there are edges that have sent at least one signal already.
 *
 *  @param id unique vertex id.
 *  @param state the initial state of the vertex.
 *
 *  @author Philip Stutz
 */
abstract class DataGraphVertex[Id, State](id: Id, state: State) extends DataGraphVertexEx[Id, State, Any, Any](id, state) {}

abstract class DataGraphVertexEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound](
  val id: Id,
  var state: State) extends AbstractVertexEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound] with SumOfOutWeightsEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound] {

  type Signal

  override def setState(s: State) {
    state = s
  }

  /**
   *  The abstract `collect` function is algorithm specific and calculates the new vertex state.
   *
   *  @note If the edge along which a signal was sent is relevant, then mostRecentSignalMap can be used to access the edge id of a signal.
   *
   *  @param mostRecentSignals An iterable that returns the most recently received signal for each edge that has sent at least one signal already.
   *
   *  @return The new vertex state.
   *
   *  @note Beware of modifying and returning a referenced object,
   *  default signal scoring and termination detection fail in this case.
   */
  def collect: State

  /**
   *  A map that has sender ids (by default the id of the source vertex) as keys and stores the most recent signal
   *  received along the edge with that id as the value for that key.
   *
   *  Currently a Java HashMap is used as the implementation, but we will replace it with a more specialized
   *  implementation in a future release.
   */
  protected var mostRecentSignalMap = Map.empty[GraphIdUpperBound, Signal]

  override def deliverSignalWithSourceId(signal: GraphSignalUpperBound, sourceId: GraphIdUpperBound, graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]): Boolean = {
    mostRecentSignalMap += ((sourceId, signal.asInstanceOf[Signal]))
    false
  }

  override def deliverSignalWithoutSourceId(signal: GraphSignalUpperBound, graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]): Boolean = {
    throw new Exception("Data graph vertices only make sense if the source id is known.")
    false
  }

  def signals = mostRecentSignalMap.values

  /**
   *  Function that gets called by the framework whenever this vertex is supposed to collect new signals.
   *
   *  @param graphEditor an instance of GraphEditor which can be used by this vertex to interact with the graph.
   */
  override def executeCollectOperation(graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]) {
    super.executeCollectOperation(graphEditor)
    setState(collect)
  }

  /**
   * This method is used by the framework in order to decide if the vertex' collect operation
   * should be executed.
   *
   * @return the score value. The meaning of this value depends on the thresholds set in the framework.
   */
  override def scoreCollect: Double = {
    if (!mostRecentSignalMap.isEmpty) {
      1.0
    } else if (edgesModifiedSinceCollectOperation) {
      1.0
    } else {
      0.0
    }
  }

  /**
   * Exposing the vertex details on a DataGraphVertexEx shows its
   * mostRecentSignalMap.
   * @return a map with a single element, the mostRecentSignalMap
   */
  override def expose: Map[String, Any] = {
    Map(("mostRecentSignalMap", makeExposable(mostRecentSignalMap.toMap)))
  }

  /**
   * Helper method to expose meaningful information no matter what types are used
   * for the signals. Recursively modifies any data structure to be compatible.
   * @param an arbitrary data structure for transformation
   * @return a transformed data structure from the one provided
   */
  private def makeExposable(a: Any): Any = {
    a match {
      case x: Array[_] => x.toList.map(makeExposable(_))
      case x: List[_] => x.map(makeExposable(_))
      case x @ (_: Int | _: Long | _: String | _: Double) => x
      case x: Map[_, _] =>
        (for ((k, v) <- x) yield {
          (k.toString -> makeExposable(v))
        })
      case other => other.toString
    }
  }

}
