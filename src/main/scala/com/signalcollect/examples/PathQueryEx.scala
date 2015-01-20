/*
 *  @author Daniel Strebel
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

package com.signalcollect.examples

import scala.collection.immutable.Queue

import com.signalcollect._

/**
 * Elements of a path query.
 * PathQueryNodes represent nodes in a query path that can match for a node in the graph.
 */
abstract class PathQueryNodeEx extends Serializable {
  def matches(vertex: Vertex[_, _, _, _]): Boolean
  def expand: List[PathQueryNodeEx] = List()
}

/**
 * PathQueryNode for which the provided condition specifies whether a node matches this query node or not.
 */
class WildcardQueryNodeEx(condition: Vertex[_, _, _, _] => Boolean = vertex => true) extends PathQueryNodeEx {
  def matches(vertex: Vertex[_, _, _, _]) = condition(vertex)
}

/**
 * More generalized version of WildcardQueryNode that can match 0 to (including) maxExpansion times in a row.
 */
class StarQueryNodeEx(condition: Vertex[_, _, _, _] => Boolean = vertex => true, maxExpansion: Int = 1) extends WildcardQueryNodeEx(condition) {
  override def expand: List[PathQueryNodeEx] = {
    if (maxExpansion > 0) {
      List(new StarQueryNodeEx(condition, maxExpansion - 1))
    } else {
      List()
    }
  }
}

/**
 * Query node that only matches a vertex if it has the specified id.
 */
class FixedQueryNodeEx(id: Any) extends PathQueryNodeEx {
  def matches(vertex: Vertex[_, _, _, _]) = (vertex.id.hashCode() == id.hashCode())
}

/**
 * A PathQuery is a chain of PathQueryNodes that specify which conditions a path through the graph
 * must fulfill in order to match the query. As the query is passed along matching nodes the matched
 * PathQueryNodes are removed from the active path query and stored in the matched path queue to keep track
 * of the matched nodes in the graph.
 */
class PathQueryEx() extends Serializable {
  var unmatchedQuery = List[PathQueryNodeEx]() //Part of the query that is not matched yet.
  var matchedPath = Queue[Any]() // Trail of already matched nodes

  /**
   * Match the head of the query to the provided vertex. If the match was successful a list follow-up queries is returned.
   *
   * @param vertex that should be matched to the head of the remaining path query.
   * @return a list of remaining queries after matching a vertex to the head of the unmatched path query or None if the head did not match the vertex.
   */
  def getRemainingQuery(vertex: Vertex[_, _, _, _]): Option[List[PathQueryEx]] = {
    if (unmatchedQuery.size > 0 && unmatchedQuery.head.matches(vertex)) {
      val remainingQuery = new PathQueryEx
      remainingQuery.matchedPath = matchedPath.enqueue(vertex.id)
      remainingQuery.unmatchedQuery = unmatchedQuery.tail

      val expandedQueryHeads = unmatchedQuery.head.expand
      val expandedQueries = expandedQueryHeads.map(queryHead => {
        val expandedQuery = new PathQueryEx
        expandedQuery.matchedPath = remainingQuery.matchedPath
        expandedQuery.prependQueryNode(queryHead)
        expandedQuery
      })
      Some(remainingQuery :: expandedQueries)
    } else {
      None
    }
  }

  /**
   * Adds a PathQueryNode to the end of the unmatched query
   */
  def appendQueryNode(node: PathQueryNodeEx) {
    unmatchedQuery = unmatchedQuery :+ node
  }

  /**
   * Adds a PathQueryNode to the beginning of the unmatched query
   */
  def prependQueryNode(node: PathQueryNodeEx) {
    unmatchedQuery = node :: unmatchedQuery
  }
}

/**
 * Collects all matched paths as results of the query.
 */
object ResultHandlerEx {
  var results = List[List[Any]]()
  def addPath(path: List[Any]) = results = path :: results 
  def getResults = results.toList
}

/**
 * Collects all matched Paths
 */
class QueryNodeEx

class QueryVertexEx(vertexId: Int, initialState: List[PathQueryEx]) extends DataFlowVertexEx[Int, List[PathQueryEx], Int, List[PathQueryEx]](vertexId, initialState) with ResetStateAfterSignalingEx[Int, List[PathQueryEx], Int, List[PathQueryEx]] {

  type Signal = List[PathQueryEx]

  val resetState = null

  def collect(queries: List[PathQueryEx]): List[PathQueryEx] = {
    var newState = state
    if (queries != null) {
      for (query <- queries) {
        if (query != null) {
          query.getRemainingQuery(this) match {
            case Some(restQueries) => {
              for (restQuery <- restQueries) {
                if (restQuery.unmatchedQuery.size == 0) {
                  ResultHandlerEx.addPath(restQuery.matchedPath.toList)
                } else {
                  if (state != null) {
                    newState = restQuery +: newState
                  } else {
                    newState = List(restQuery)
                  }

                }
              }

            }
            case _ =>
          }
        }
      }
    }
    newState
  }
}

/**
 * A little demo that builds a graph and looks for paths
 */
object PathQueryExampleEx extends App {

  val graph = new GraphBuilder[Int, List[PathQueryEx]].build
  val query = new PathQueryEx
  //  query.addQueryNode(new WildcardQueryNodeEx)
  //  query.addQueryNode(new WildcardQueryNodeEx)
  //  query.addQueryNode(new FixedQueryNodeEx(3))
  //  query.addQueryNode(new WildcardQueryNodeEx)
  //  query.addQueryNode(new FixedQueryNodeEx(2))
  //  query.addQueryNode(new WildcardQueryNodeEx)
  query.appendQueryNode(new StarQueryNodeEx(maxExpansion = 5))
  query.appendQueryNode(new FixedQueryNodeEx(2))

  graph.addVertex(new QueryVertexEx(0, List(query)))
  graph.addVertex(new QueryVertexEx(1, null))
  graph.addVertex(new QueryVertexEx(2, null))
  graph.addVertex(new QueryVertexEx(3, null))
  graph.addVertex(new QueryVertexEx(4, null))
  graph.addVertex(new QueryVertexEx(5, null))

  graph.addEdge(0, new StateForwarderEdge(1))
  graph.addEdge(0, new StateForwarderEdge(2))
  graph.addEdge(1, new StateForwarderEdge(2))
  graph.addEdge(2, new StateForwarderEdge(3))
  graph.addEdge(3, new StateForwarderEdge(4))
  graph.addEdge(4, new StateForwarderEdge(2))
  graph.addEdge(2, new StateForwarderEdge(5))

  val stats = graph.execute

  println(ResultHandlerEx.getResults)

  graph.shutdown
}