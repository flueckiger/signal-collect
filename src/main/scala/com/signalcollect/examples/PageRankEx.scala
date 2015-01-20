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

package com.signalcollect.examples

import com.signalcollect._
import com.signalcollect.configuration.ExecutionMode

/**
 * Represents an edge in a PageRank compute graph
 *
 *  @param s: the identifier of the source vertex
 *  @param t: the identifier of the target vertex
 */
class PageRankEdgeEx[Id](t: Id) extends DefaultEdge(t) {

  type Source = PageRankVertexEx[Id]

  /**
   * The signal function calculates how much rank the source vertex
   *  transfers to the target vertex.
   */
  def signal = source.state * weight / source.sumOfOutWeights

}

/**
 * Represents a page in a PageRank compute graph
 *
 *  @param id: the identifier of this vertex
 *  @param dampingFactor: @see <a href="http://en.wikipedia.org/wiki/PageRank">PageRank algorithm</a>
 */
class PageRankVertexEx[Id](id: Id, dampingFactor: Double = 0.85) extends DataGraphVertexEx[Id, Double, Id, Double](id, 1 - dampingFactor) {

  type Signal = Double

  /**
   * The collect function calculates the rank of this vertex based on the rank
   *  received from neighbors and the damping factor.
   */
  def collect: Double = 1 - dampingFactor + dampingFactor * signals.sum

  override def scoreSignal: Double = {
    if (edgesModifiedSinceSignalOperation) {
      1
    } else {
      lastSignalState match {
        case None => 1
        case Some(oldState) => (state - oldState).abs
      }
    }
  }

}

/** Builds a PageRank compute graph and executes the computation */
object PageRankEx extends App {
  val graph = new GraphBuilder[Int, Double]().
    //withConsole(true).
    build

  graph.awaitIdle
  graph.addVertex(new PageRankVertexEx(1))
  graph.addVertex(new PageRankVertexEx(2))
  graph.addVertex(new PageRankVertexEx(3))
  graph.addEdge(1, new PageRankEdgeEx(2))
  graph.addEdge(2, new PageRankEdgeEx(1))
  graph.addEdge(2, new PageRankEdgeEx(3))
  graph.addEdge(3, new PageRankEdgeEx(2))

  graph.awaitIdle
  val stats = graph.execute//(ExecutionConfiguration.withExecutionMode(ExecutionMode.Interactive))
  println(stats)

  graph.foreachVertex(println(_))
  graph.shutdown
}
