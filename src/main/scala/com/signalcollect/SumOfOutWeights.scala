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

trait SumOfOutWeights[Id, State] extends SumOfOutWeightsEx[Id, State, Any, Any] {}

trait SumOfOutWeightsEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound] extends AbstractVertexEx[Id, State, GraphIdUpperBound, GraphSignalUpperBound] {

  /**
   * @return The sum of the weights of all outgoing edges.
   */
  var sumOfOutWeights: Double = 0

  abstract override def addEdge(e: Edge[GraphIdUpperBound], graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]): Boolean = {
    val added = super.addEdge(e, graphEditor)
    if (added) {
      sumOfOutWeights += e.weight
    }
    added
  }

  abstract override def removeEdge(targetId: GraphIdUpperBound, graphEditor: GraphEditor[GraphIdUpperBound, GraphSignalUpperBound]): Boolean = {
    val outgoingEdge = outgoingEdges.get(targetId)
    val weightToSubtract = outgoingEdge match {
      case None => 0
      case Some(edge) => edge.weight
    }
    val removed = super.removeEdge(targetId, graphEditor)
    if (removed) {
      sumOfOutWeights -= weightToSubtract
    }
    removed
  }

}