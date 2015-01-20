package com.signalcollect

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.signalcollect._
import com.signalcollect.util.TestAnnouncements
import com.signalcollect.examples.PageRankVertexEx
import com.signalcollect.examples.PageRankEdgeEx
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandlerFactory
import com.signalcollect.interfaces.EdgeAddedToNonExistentVertexHandler

class TestEdgeAddedToNonExistentVertexHandlerFactoryEx extends EdgeAddedToNonExistentVertexHandlerFactory[Any, Double] {
  def createInstance: EdgeAddedToNonExistentVertexHandler[Any, Double] =
    new TestEdgeAddedToNonExistentVertexHandlerEx
  override def toString = "TestEdgeAddedToNonExistentVertexHandlerFactoryEx"
}

class TestEdgeAddedToNonExistentVertexHandlerEx extends EdgeAddedToNonExistentVertexHandler[Any, Double] {
  def handleImpossibleEdgeAddition(edge: Edge[Any], vertexId: Any): Option[Vertex[Any, _, Any, Double]] = {
    val v = new PageRankVertexEx[Any](vertexId)
    Some(v)
  }
}

class NonExistentVertexHandlerSpecEx extends FlatSpec with Matchers with TestAnnouncements {

  "Handler for adding an edge to a nonexistent vertex" should "correctly create vertices if needed" in {
    val g = new GraphBuilder[Any, Double].withEdgeAddedToNonExistentVertexHandlerFactory(new TestEdgeAddedToNonExistentVertexHandlerFactoryEx).build
    try {
      g.addEdge(1, new PageRankEdgeEx(2))
      g.addEdge(2, new PageRankEdgeEx(1))
      val stats = g.execute
      assert(stats.aggregatedWorkerStatistics.numberOfVertices == 2)
    } finally {
      g.shutdown
    }
  }

}
