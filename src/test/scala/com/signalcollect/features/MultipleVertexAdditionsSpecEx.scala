/*
 *  @author Philip Stutz
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

package com.signalcollect.features

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import com.signalcollect.util.IntSet
import com.signalcollect.util.Ints
import com.signalcollect._
import com.signalcollect.examples.PageRankVertexEx
import org.scalatest.Matchers
import akka.event.Logging
import com.signalcollect.interfaces.ExistingVertexHandlerFactory
import com.signalcollect.interfaces.ExistingVertexHandler
import com.signalcollect.util.TestAnnouncements

class DummyVertexEx(id: Int) extends PageRankVertexEx(id) {
  state = 1
}

class TestExistingVertexHandlerFactoryEx[Id, Signal] extends ExistingVertexHandlerFactory[Id, Signal] {
  def createInstance: ExistingVertexHandler[Id, Signal] =
    new TestExistingVertexHandlerEx[Id, Signal]
  override def toString = "TestExistingVertexHandlerFactoryEx"
}

class TestExistingVertexHandlerEx[Id, Signal] extends ExistingVertexHandler[Id, Signal] {
  def mergeVertices(existing: Vertex[Id, _, Id, Signal], failedVertexAddition: Vertex[Id, _, Id, Signal], ge: GraphEditor[Id, Signal]) {
    existing.asInstanceOf[DummyVertexEx].state += 1.0
  }
}

class MultipleVertexAdditionsSpecEx extends FlatSpec with Matchers with TestAnnouncements {

  "Adding the same vertex multiple times" should "be ignored" in {
    val g = new GraphBuilder[Int, Double].build //.withLoggingLevel(Logging.DebugLevel)
    try {
      g.addVertex(new DummyVertexEx(133))
      g.addVertex(new DummyVertexEx(134))
      g.addVertex(new DummyVertexEx(133))
      val numberOfDummies = g.aggregate(SumOfStates[Double])
      numberOfDummies.get should equal(2.0)
    } finally {
      g.shutdown
    }
  }

  it should "support merges via handler" in {
    val g = new GraphBuilder[Int, Double].withExistingVertexHandlerFactory(new TestExistingVertexHandlerFactoryEx[Int, Double]).build
    try {
      g.addVertex(new DummyVertexEx(133))
      g.addVertex(new DummyVertexEx(134))
      g.addVertex(new DummyVertexEx(133))
      val stateSum = g.aggregate(SumOfStates[Double])
      stateSum.get should equal(3.0)
    } finally {
      g.shutdown
    }
  }

}