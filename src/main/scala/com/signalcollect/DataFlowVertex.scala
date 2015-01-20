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

/**
 *  Vertex implementation that collects all the signals that have arrived since the last
 *  time this vertex has collected. Users of the framework extend this class to implement
 *  a specific algorithm by defining a `collect` function.
 *
 *  @note The `collect` function receives all signals that arrived at this vertex but have not
 *  been collected yet as a parameter.
 *
 *  @param id Unique vertex id.
 *  @param state The initial state of the vertex.
 *  @param resetState The state will be set to `resetState` after signaling.
 *
 *  @author Philip Stutz
 */
abstract class DataFlowVertex[Id, State](id: Id, state: State) extends DataFlowVertexEx[Id, State, Any, Any](id, state) {}

abstract class DataFlowVertexEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound](
  val id: Id,
  var state: State)
  extends AbstractVertexEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound] {

  type Signal

  def setState(s: State) {
    state = s
  }

  def deliverSignalWithSourceId(signal: GraphSignalUpperBound, sourceId: GraphIdUpperBound, graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]): Boolean = {
    deliverSignalWithoutSourceId(signal, graphEditor)
  }

  def deliverSignalWithoutSourceId(signal: GraphSignalUpperBound, graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]): Boolean = {
    setState(collect(signal.asInstanceOf[Signal]))
    true
  }

  /**
   *  The abstract `collect` function is algorithm specific and calculates the new vertex state.
   *
   *  @param signal a signal that was received by this vertex and has not yet been collected.
   *
   *  @return The new vertex state.
   *
   *  @note Beware of modifying and returning a referenced object,
   *  default signal scoring and termination detection fail in this case.
   */
  def collect(signal: Signal): State

  /**
   *  Function that gets called by the framework whenever this vertex is supposed to collect new signals.
   *
   *  @param graphEditor can be used by this vertex to interact with the graph.
   */
  override def executeCollectOperation(graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]) {
    super.executeCollectOperation(graphEditor)
  }

  /**
   * This method is used by the framework in order to decide if the vertex' collect operation
   * should be executed.
   *
   * @return the score value. The meaning of this value depends on the thresholds set in the framework.
   */
  def scoreCollect: Double = {
    if (edgesModifiedSinceCollectOperation) {
      1.0
    } else {
      0.0
    }
  }

}
