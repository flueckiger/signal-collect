/*
 *  @author Philip Stutz
 *
 *  Copyright 2013 University of Zurich
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

package com.signalcollect.examples

import com.signalcollect._

class ClusteringCoefficientVertexEx(id: Any) extends DataGraphVertexEx[Any, Double, Any, Set[Any]](id, 0.0) {

  type Signal = Set[Any]

  lazy val maxEdges = outgoingEdges.size * (outgoingEdges.size - 1)
  lazy val neighbourIds = targetIds.toSet

  def collect = {
    if (maxEdges != 0) {
      val edgesBetweenNeighbours = signals.map(neighbourIds.intersect(_)).map(_.size).sum
      edgesBetweenNeighbours / maxEdges.toDouble
    } else {
      Double.NaN
    }
  }

}

class ClusteringCoefficientEdgeEx(t: Any) extends DefaultEdge(t) {
  type Source = ClusteringCoefficientVertexEx

  def signal = source.neighbourIds

}

object ClusteringCoefficientEx extends App {
  val graph = new GraphBuilder[Any, Set[Any]].build
  graph.addVertex(new ClusteringCoefficientVertexEx(1))
  graph.addVertex(new ClusteringCoefficientVertexEx(2))
  graph.addVertex(new ClusteringCoefficientVertexEx(3))
  graph.addVertex(new ClusteringCoefficientVertexEx(4))
  graph.addVertex(new ClusteringCoefficientVertexEx(5))
  graph.addVertex(new ClusteringCoefficientVertexEx(6))
  graph.addEdge(1, new ClusteringCoefficientEdgeEx(2))
  graph.addEdge(2, new ClusteringCoefficientEdgeEx(1))
  graph.addEdge(1, new ClusteringCoefficientEdgeEx(3))
  graph.addEdge(3, new ClusteringCoefficientEdgeEx(1))
  graph.addEdge(1, new ClusteringCoefficientEdgeEx(4))
  graph.addEdge(4, new ClusteringCoefficientEdgeEx(1))
  graph.addEdge(1, new ClusteringCoefficientEdgeEx(5))
  graph.addEdge(5, new ClusteringCoefficientEdgeEx(1))
  graph.addEdge(2, new ClusteringCoefficientEdgeEx(3))
  graph.addEdge(3, new ClusteringCoefficientEdgeEx(2))
  graph.addEdge(3, new ClusteringCoefficientEdgeEx(5))
  graph.addEdge(5, new ClusteringCoefficientEdgeEx(3))
  graph.addEdge(6, new ClusteringCoefficientEdgeEx(5))
  graph.addEdge(5, new ClusteringCoefficientEdgeEx(6))
  graph.addEdge(6, new ClusteringCoefficientEdgeEx(1))
  graph.addEdge(1, new ClusteringCoefficientEdgeEx(6))

  val stats = graph.execute
  println(stats)
  graph.foreachVertex(println(_))
  graph.shutdown
}
