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

package com.signalcollect.storage

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import com.signalcollect.examples.PageRankVertexEx
import com.signalcollect.examples.SudokuCellEx
import com.signalcollect.Vertex

@org.junit.Ignore
@RunWith(classOf[JUnitRunner])
class VertexMapSpecEx extends SpecificationWithJUnit with Mockito {

  sequential

  "VertexMap" should {

    "support puts" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(1))
      vm.put(new PageRankVertexEx(2))
      vm.put(new PageRankVertexEx(3))
      vm.isEmpty must_== false
      vm.size must_== 3
    }

    // There was a bug, where the map was not optimized after processing.
    "optimize after processing one item" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(0))
      vm.put(new PageRankVertexEx(1))
      vm.put(new PageRankVertexEx(2))
      vm.put(new PageRankVertexEx(3))
      vm.put(new PageRankVertexEx(4))
      vm.put(new PageRankVertexEx(5))
      vm.put(new PageRankVertexEx(6))
      vm.put(new PageRankVertexEx(16))
      vm.process(v => {}, Some(1))
      vm.get(16) != null
    }

    "optimize after processing several items" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(0))
      vm.put(new PageRankVertexEx(1))
      vm.put(new PageRankVertexEx(2))
      vm.put(new PageRankVertexEx(3))
      vm.put(new PageRankVertexEx(4))
      vm.put(new PageRankVertexEx(5))
      vm.put(new PageRankVertexEx(6))
      vm.put(new PageRankVertexEx(16))
      vm.process(v => {}, Some(4))
      vm.process(v => {}, Some(3))
      vm.get(16) != null
    }

    "optimize correctly after processing several items" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(0))
      vm.put(new PageRankVertexEx(1))
      vm.put(new PageRankVertexEx(2))
      vm.put(new PageRankVertexEx(3))
      vm.put(new PageRankVertexEx(4))
      vm.put(new PageRankVertexEx(5))
      vm.put(new PageRankVertexEx(16))
      vm.put(new PageRankVertexEx(17))
      vm.process(v => {}, Some(6))
      vm.get(17) != null
    }

    "optimize correctly in an edge case" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(0))
      vm.put(new PageRankVertexEx(1))
      vm.put(new PageRankVertexEx(2))
      vm.put(new PageRankVertexEx(3))
      vm.put(new PageRankVertexEx(4))
      vm.put(new PageRankVertexEx(5))
      vm.put(new PageRankVertexEx(16))
      vm.put(new PageRankVertexEx(6))
      vm.process(v => {}, Some(6))
      vm.get(6) != null
    }

    "remove all elements via processing" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(0))
      vm.put(new PageRankVertexEx(1))
      vm.put(new PageRankVertexEx(2))
      vm.put(new PageRankVertexEx(3))
      vm.put(new PageRankVertexEx(4))
      vm.put(new PageRankVertexEx(5))
      vm.put(new PageRankVertexEx(6))
      vm.put(new PageRankVertexEx(16))
      vm.process(v => {}, Some(5))
      vm.process(v => {}, Some(3))
      vm.size == 0
    }

    "handle special keys" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new PageRankVertexEx(0))
      vm.put(new PageRankVertexEx(Int.MinValue))
      vm.put(new PageRankVertexEx(Int.MaxValue))
      vm.size must_== 3
      vm.get(0).id.asInstanceOf[Int] must_== 0
      vm.get(Int.MinValue).id must_== Int.MinValue
      vm.get(Int.MaxValue).id must_== Int.MaxValue
    }

    case class Ab(a: Int, b: Int) {
      override def hashCode = a.hashCode
      override def equals(other: Any) = a.equals(other)
    }

    "fail to retrieve a vertex with the same hash code and different object" in {
      val vm = new VertexMap[Ab, Double](8, 0.99f)
      val v1 = new PageRankVertexEx(Ab(1, 100))
      val v2 = new PageRankVertexEx(Ab(1, 101))
      vm.put(v1)
      vm.get(v1.id) === v1
      vm.get(v2.id) === null
    }

    "handle hash collisions correctly" in {
      val vm = new VertexMap[Ab, Double](8, 0.99f)
      val v1 = new PageRankVertexEx(Ab(1, 100))
      val v2 = new PageRankVertexEx(Ab(1, 101))
      vm.put(v1)
      vm.put(v2)
      vm.get(v1.id) === v1
      vm.get(v2.id) === v2
    }

    "stream vertices" in {
      val vm = new VertexMap[Int, Double](8, 0.99f)
      vm.put(new SudokuCellEx(0)) === true
      vm.put(new SudokuCellEx(Int.MinValue)) === true
      vm.put(new SudokuCellEx(Int.MaxValue)) === true
      vm.size must_== 3
      (vm.stream.toList map (_.id)) === List(0, Int.MinValue, Int.MaxValue)
    }

  }

}