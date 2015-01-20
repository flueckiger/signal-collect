/*
 *  @author Daniel Strebel
 *  @author Philip Stutz
 *
 *  Copyright 2011 University of Zurich
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

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import com.signalcollect.configuration.ExecutionMode
import com.signalcollect.examples.ColoredVertexEx
import com.signalcollect.examples.LocationEx
import com.signalcollect.examples.PageRankEdgeEx
import com.signalcollect.examples.PageRankVertexEx
import com.signalcollect.examples.PathEx
import org.specs2.runner.JUnitRunner
import com.signalcollect.interfaces.ModularAggregationOperation
import akka.event.Logging

/**
 * Hint: For information on how to run specs see the specs v.1 website
 * http://code.google.com/p/specs/wiki/RunningSpecs
 */
@RunWith(classOf[JUnitRunner])
class IntegrationSpecEx extends SpecificationWithJUnit with Serializable {

  sequential

  def computeGraphFactories[Id: ClassTag: TypeTag, Signal: ClassTag: TypeTag]: List[() => Graph[Id, Signal]] = List(() => new GraphBuilder[Id, Signal].
    //      withMessageSerialization(true).
    //      withLoggingLevel(Logging.DebugLevel).
    build)

  val executionModes = List(ExecutionMode.Synchronous, ExecutionMode.OptimizedAsynchronous)

  def test[Id: ClassTag: TypeTag, Signal: ClassTag: TypeTag](graphProviders: List[() => Graph[Id, Signal]] = null, verify: Vertex[_, _, _, _] => Boolean, buildGraph: Graph[Id, Signal] => Unit = (graph: Graph[Id, Signal]) => (), signalThreshold: Double = 0.01, collectThreshold: Double = 0): Boolean = {
    var correct = true
    var computationStatistics = Map[String, List[ExecutionInformation[Any, Any]]]()

    for (executionMode <- executionModes) {
      for (graphProvider <- if (graphProviders eq null) computeGraphFactories[Id, Signal] else graphProviders) {
        val graph = graphProvider()
        try {
          buildGraph(graph)
          val stats = graph.execute(ExecutionConfiguration(executionMode = executionMode, signalThreshold = signalThreshold))
          correct &= graph.aggregate(new ModularAggregationOperation[Boolean] {
            val neutralElement = true
            def aggregate(a: Boolean, b: Boolean): Boolean = a && b
            def extract(v: Vertex[_, _, _, _]): Boolean = verify(v)
          })
          if (!correct) {
            System.err.println("Test failed. Computation stats: " + stats)
          }
        } finally {
          graph.shutdown
        }
      }
    }
    correct
  }

  def buildPageRankGraph(graph: Graph[Int, Double], edgeTuples: Traversable[Tuple2[Int, Int]]): Graph[Int, Double] = {
    edgeTuples foreach {
      case (sourceId: Int, targetId: Int) =>
        graph.addVertex(new PageRankVertexEx(sourceId, 0.85))
        graph.addVertex(new PageRankVertexEx(targetId, 0.85))
        graph.addEdge(sourceId, new PageRankEdgeEx(targetId))
    }
    graph
  }

  def buildVertexColoringGraph(numColors: Int, graph: Graph[Any, Int], edgeTuples: Traversable[Tuple2[Int, Int]]): Graph[Any, Int] = {
    edgeTuples foreach {
      case (sourceId, targetId) =>
        graph.addVertex(new VerifiedColoredVertexEx(sourceId, numColors))
        graph.addVertex(new VerifiedColoredVertexEx(targetId, numColors))
        graph.addEdge(sourceId, new StateForwarderEdge(targetId))
    }
    graph
  }

  def buildSsspGraph(pathSourceId: Any, graph: Graph[Any, Int], edgeTuples: Traversable[Tuple2[Int, Int]]): Graph[Any, Int] = {
    edgeTuples foreach {
      case (sourceId, targetId) =>
        if (sourceId.equals(pathSourceId)) {
          graph.addVertex(new LocationEx(sourceId, Some(0)))
        } else {
          graph.addVertex(new LocationEx(sourceId, None))
        }
        if (targetId.equals(pathSourceId)) {
          graph.addVertex(new LocationEx(targetId, Some(0)))
        } else {
          graph.addVertex(new LocationEx(targetId, None))
        }
        graph.addEdge(sourceId, new PathEx(targetId))
    }
    graph
  }

  "PageRank algorithm" should {
    "deliver correct results on a 5-cycle graph" in {
      val fiveCycleEdges = List((0, 1), (1, 2), (2, 3), (3, 4), (4, 0))
      def pageRankFiveCycleVerifier(v: Vertex[_, _, _, _]): Boolean = {
        val state = v.state.asInstanceOf[Double]
        val expectedState = 1.0
        val correct = (state - expectedState).abs < 0.01
        if (!correct) {
          System.err.println("Problematic vertex:  id=" + v.id + ", expected state=" + expectedState + ", actual state=" + state)
        }
        correct
      }
      test[Int, Double](verify = pageRankFiveCycleVerifier, buildGraph = buildPageRankGraph(_, fiveCycleEdges), signalThreshold = 0.001) must_== true
    }

    "deliver correct results on a 5-star graph" in {
      val fiveStarEdges = List((0, 4), (1, 4), (2, 4), (3, 4))
      def pageRankFiveStarVerifier(v: Vertex[_, _, _, _]): Boolean = {
        val state = v.state.asInstanceOf[Double]
        val expectedState = if (v.id == 4.0) 0.66 else 0.15
        val correct = (state - expectedState).abs < 0.00001
        if (!correct) {
          System.err.println("Problematic vertex:  id=" + v.id + ", expected state=" + expectedState + ", actual state=" + state)
        }
        correct
      }
      test[Int, Double](verify = pageRankFiveStarVerifier, buildGraph = buildPageRankGraph(_, fiveStarEdges)) must_== true
    }

    "deliver correct results on a 2*2 symmetric grid" in {
      val symmetricTwoOnTwoGridEdges = new GridEx(2, 2)
      def pageRankTwoOnTwoGridVerifier(v: Vertex[_, _, _, _]): Boolean = {
        val state = v.state.asInstanceOf[Double]
        val expectedState = 1.0
        val correct = (state - expectedState).abs < 0.01
        if (!correct) {
          System.err.println("Problematic vertex:  id=" + v.id + ", expected state=" + expectedState + ", actual state=" + state)
        }
        correct
      }
      test[Int, Double](verify = pageRankTwoOnTwoGridVerifier, buildGraph = buildPageRankGraph(_, symmetricTwoOnTwoGridEdges), signalThreshold = 0.001) must_== true
    }

    "deliver correct results on a 5*5 torus" in {
      val symmetricTorusEdges = new TorusEx(5, 5)
      def pageRankTorusVerifier(v: Vertex[_, _, _, _]): Boolean = {
        val state = v.state.asInstanceOf[Double]
        val expectedState = 1.0
        val correct = (state - expectedState).abs < 0.01
        if (!correct) {
          System.err.println("Problematic vertex:  id=" + v.id + ", expected state=" + expectedState + ", actual state=" + state)
        }
        correct
      }
      test[Int, Double](verify = pageRankTorusVerifier, buildGraph = buildPageRankGraph(_, symmetricTorusEdges), signalThreshold = 0.001) must_== true
    }
  }

  def vertexColoringVerifier(v: Vertex[_, _, _, _]): Boolean = {
    v match {
      case v: VerifiedColoredVertexEx =>
        val verified = !v.publicMostRecentSignals.iterator.contains(v.state)
        if (!verified) {
          System.err.println("Vertex Coloring: " + v + " has the same color as one of its neighbors.\n" +
            "Most recent signals received: " + v.publicMostRecentSignals + "\n" +
            "Score signal: " + v.scoreSignal)
        }
        verified
      case other =>
        System.err.println("Vertex " + other + " is not of type VerifiedColoredVertexEx"); false
    }
  }

  "VertexColoring algorithm" should {
    "deliver correct results on a symmetric 4-cycle" in {
      val symmetricFourCycleEdges = List((0, 1), (1, 0), (1, 2), (2, 1), (2, 3), (3, 2), (3, 0), (0, 3))
      test[Any, Int](verify = vertexColoringVerifier, buildGraph = buildVertexColoringGraph(2, _, symmetricFourCycleEdges)) must_== true
    }

    "deliver correct results on a symmetric 5-star" in {
      val symmetricFiveStarEdges = List((0, 4), (4, 0), (1, 4), (4, 1), (2, 4), (4, 2), (3, 4), (4, 3))
      test[Any, Int](verify = vertexColoringVerifier, buildGraph = buildVertexColoringGraph(2, _, symmetricFiveStarEdges)) must_== true
    }
    "deliver correct results on a 2*2 symmetric grid" in {
      val symmetricTwoOnTwoGridEdges = new GridEx(2, 2)
      test[Any, Int](verify = vertexColoringVerifier, buildGraph = buildVertexColoringGraph(2, _, symmetricTwoOnTwoGridEdges)) must_== true
    }
  }

  "SSSP algorithm" should {
    "deliver correct results on a symmetric 4-cycle" in {
      val symmetricFourCycleEdges = List((0, 1), (1, 2), (2, 3), (3, 0))
      def ssspSymmetricsFourCycleVerifier(v: Vertex[_, _, _, _]): Boolean = {
        val state = v.state.asInstanceOf[Option[Int]].get
        val expectedState = v.id
        val correct = state == expectedState
        if (!correct) {
          System.err.println("Problematic vertex:  id=" + v.id + ", expected state=" + expectedState + ", actual state=" + state)
        }
        correct
      }
      test[Any, Int](verify = ssspSymmetricsFourCycleVerifier, buildGraph = buildSsspGraph(0, _, symmetricFourCycleEdges)) must_== true
    }

    "deliver correct results on a symmetric 5-star" in {
      val symmetricFiveStarEdges = List((0, 4), (4, 0), (1, 4), (4, 1), (2, 4), (4, 2), (3, 4), (4, 3))
      def ssspSymmetricFiveStarVerifier(v: Vertex[_, _, _, _]): Boolean = {
        val state = v.state.asInstanceOf[Option[Int]].get
        val expectedState = if (v.id == 4) 0 else 1
        val correct = state == expectedState
        if (!correct) {
          System.err.println("Problematic vertex:  id=" + v.id + ", expected state=" + expectedState + ", actual state=" + state)
        }
        correct
      }
      test[Any, Int](verify = ssspSymmetricFiveStarVerifier, buildGraph = buildSsspGraph(4, _, symmetricFiveStarEdges)) must_== true
    }

  }
}

class VerifiedColoredVertexEx(id: Int, numColors: Int) extends ColoredVertexEx(id, numColors, 0, false) {
  // only necessary to allow access to vertex internals
  def publicMostRecentSignals: Iterable[Int] = mostRecentSignalMap.values
}

/**
 * Required for integration testing. Returns an undirected grid-structured graph.
 * Example GridEx(2,2): Edges=(1,3), (3,1), (1,2), (2,1), (2,4), (4,2), (3,4), (4,3)
 * 		1-2
 * 		| |
 * 		3-4
 */
class GridEx(val width: Int, height: Int) extends Traversable[(Int, Int)] with Serializable {

  def foreach[U](f: ((Int, Int)) => U) = {
    val max = width * height
    for (n <- 1 to max) {
      if (n + width <= max) {
        f((n, n + width))
        f((n + width, n))
      }
      if (n % height != 0) {
        f((n, n + 1))
        f((n + 1, n))
      }
    }
  }
}

class TorusEx(val width: Int, height: Int) extends Traversable[(Int, Int)] with Serializable {

  def foreach[U](f: ((Int, Int)) => U) = {
    val max = width * height
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val flattenedCurrentId = flatten((x, y), width)
        for (neighbor <- neighbors(x, y, width, height).map(flatten(_, width))) {
          f((flattenedCurrentId, neighbor))
        }
      }
    }
  }

  def neighbors(x: Int, y: Int, width: Int, height: Int): List[(Int, Int)] = {
    List(
      (x, decrease(y, height)),
      (decrease(x, width), y), (increase(x, width), y),
      (x, increase(y, height)))
  }

  def decrease(counter: Int, limit: Int): Int = {
    if (counter - 1 >= 0) {
      counter - 1
    } else {
      width - 1
    }
  }

  def increase(counter: Int, limit: Int): Int = {
    if (counter + 1 >= width) {
      0
    } else {
      counter + 1
    }
  }

  def flatten(coordinates: (Int, Int), width: Int): Int = {
    coordinates._1 + coordinates._2 * width
  }
}