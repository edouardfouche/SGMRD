/*
 * Copyright (C) 2018 Edouard Fouché
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

import io.github.edouardfouche.index.dimension.D_Count_Stream
import org.scalatest.FunSuite

class TestCountRankStream extends FunSuite {

  // init
  test(s"Check init for odd") {
    val odd1 = new D_Count_Stream(Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0))
    assert((odd1(1.0)._1.toArray.deep, odd1(1.0)._2) == (Array(2).deep, 1))
    assert((odd1(2.0)._1.toArray.deep, odd1(2.0)._2) == (Array(0).deep, 1))
    assert((odd1(3.0)._1.toArray.deep, odd1(3.0)._2) == (Array(3).deep, 1))
    assert((odd1(4.0)._1.toArray.deep, odd1(4.0)._2) == (Array(4).deep, 1))
    assert((odd1(5.0)._1.toArray.deep, odd1(5.0)._2) == (Array(5).deep, 1))
    assert((odd1(6.0)._1.toArray.deep, odd1(6.0)._2) == (Array(1).deep, 1))
    assert((odd1(7.0)._1.toArray.deep, odd1(7.0)._2) == (Array(6).deep, 1))
    assert(odd1.dindex.keys.toSet == Set(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
  }

  test(s"Check init for ties") {
    val ties1 = new D_Count_Stream(Array(3.0, 2.0, 2.0, 1.0, 2.0))
    assert((ties1(1.0)._1.toArray.deep, ties1(1.0)._2) == (Array(3).deep, 1))
    assert((ties1(2.0)._1.toArray.deep, ties1(2.0)._2) == (Array(1, 2, 4).deep, 3))
    assert((ties1(3.0)._1.toArray.deep, ties1(3.0)._2) == (Array(0).deep, 1))
  }

  val odd = Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0)
  val even = Array(4.0, 2.0, 6.0, 1.0, 3.0, 5.0)
  test(s"Check insert below delete for odd") {
    val odd2 = new D_Count_Stream(odd)
    odd2.insert(1.5)
    assert((odd2(1.5)._1.toArray.deep, odd2(1.5)._2) == (Array(7).deep, 1))
    odd2.refresh
    assert((odd2(1.5)._1.toArray.deep, odd2(1.5)._2) == (Array(6).deep, 1))
  }


  test(s"Check insert after delete for odd") {
    val odd3 = new D_Count_Stream(odd)
    odd3.insert(2.5)
    assert((odd3(2.5)._1.toArray.deep, odd3(2.5)._2) == (Array(7).deep, 1))
    assert(odd3.dindex.getOrElse(2.0, -1) == -1)
    odd3.refresh
    assert((odd3(2.5)._1.toArray.deep, odd3(2.5)._2) == (Array(6).deep, 1))
    assert(odd3.dindex.getOrElse(2.0, -1) == -1)
  }

  // insert, ties
  val tiesA = Array(3.0, 2.0, 2.0, 1.0, 2.0)
  test(s"A - Check insert middle for ties") {
    val ties4 = new D_Count_Stream(tiesA)
    ties4.insert(2.0)
    assert((ties4(2.0)._1.toArray.deep, ties4(2.0)._2) == (Array(1, 2, 4, 5).deep, 4))
    ties4.refresh
    assert((ties4(2.0)._1.toArray.deep, ties4(2.0)._2) == (Array(0, 1, 3, 4).deep, 4))
    assert(ties4.dindex.getOrElse(3.0, -1) == -1)

  }

  val tiesD = Array(2.0, 0.0, 2.0, 1.0, 2.0)
  test(s"D - Check insert middle for ties") {
    val ties10 = new D_Count_Stream(tiesD)
    //println(ties10)
    ties10.insert(2.0)
    //println(ties10)
    assert((ties10(2.0)._1.toArray.deep, ties10(2.0)._2) == (Array(2, 4, 5).deep, 3))
    ties10.refresh()
    assert((ties10(2.0)._1.toArray.deep, ties10(2.0)._2) == (Array(1, 3, 4).deep, 3))
  }

  val tiesE = Array(2.0, 3.0, 2.0, 1.0, 2.0)
  test(s"E - Check insert below delete for ties") {
    val ties8 = new D_Count_Stream(tiesE)
    ties8.insert(1.0)
    assert((ties8(1.0)._1.toArray.deep, ties8(1.0)._2) == (Array(3, 5).deep, 2))
    assert((ties8(2.0)._1.toArray.deep, ties8(2.0)._2) == (Array(2, 4).deep, 2))
  }

  test(s"Test Scenario 1") {
    val odd1 = new D_Count_Stream(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
    odd1.insert(1.0)
    assert((odd1(0.0)._1.toArray.deep, odd1(0.0)._2) == (Array(5).deep, 1))
    assert((odd1(1.0)._1.toArray.deep, odd1(1.0)._2) == (Array(3, 4, 6).deep, 3))
    assert((odd1(2.0)._1.toArray.deep, odd1(2.0)._2) == (Array(1).deep, 1))
    assert((odd1(6.0)._1.toArray.deep, odd1(6.0)._2) == (Array(2).deep, 1))

    odd1.insert(1.0)
    assert((odd1(0.0)._1.toArray.deep, odd1(0.0)._2) == (Array(5).deep, 1))
    assert((odd1(1.0)._1.toArray.deep, odd1(1.0)._2) == (Array(3, 4, 6, 7).deep, 4))
    assert((odd1(6.0)._1.toArray.deep, odd1(6.0)._2) == (Array(2).deep, 1))
    assert(odd1.dindex.getOrElse(2.0, -1) == -1)

    odd1.insert(6.0)
    assert((odd1(0.0)._1.toArray.deep, odd1(0.0)._2) == (Array(5).deep, 1))
    assert((odd1(1.0)._1.toArray.deep, odd1(1.0)._2) == (Array(3, 4, 6, 7).deep, 4))
    assert((odd1(6.0)._1.toArray.deep, odd1(6.0)._2) == (Array(8).deep, 1))
    assert(odd1.dindex.getOrElse(2.0, -1) == -1)

    odd1.refresh
    assert((odd1(0.0)._1.toArray.deep, odd1(0.0)._2) == (Array(2).deep, 1))
    assert((odd1(1.0)._1.toArray.deep, odd1(1.0)._2) == (Array(0, 1, 3, 4).deep, 4))
    assert((odd1(6.0)._1.toArray.deep, odd1(6.0)._2) == (Array(5).deep, 1))
    assert(odd1.dindex.getOrElse(2.0, -1) == -1)
  }

  test("Test Scenario 2") {
    val odd2 = new D_Count_Stream(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
    odd2.insert(2.0)
    odd2.insert(2.0)
    odd2.insert(6.0)
    assert((odd2(0.0)._1.toArray.deep, odd2(0.0)._2) == (Array(5).deep, 1))
    assert((odd2(1.0)._1.toArray.deep, odd2(1.0)._2) == (Array(3, 4).deep, 2))
    assert((odd2(2.0)._1.toArray.deep, odd2(2.0)._2) == (Array(6, 7).deep, 2))
    assert((odd2(6.0)._1.toArray.deep, odd2(6.0)._2) == (Array(8).deep, 1))

    odd2.refresh
    assert((odd2(0.0)._1.toArray.deep, odd2(0.0)._2) == (Array(2).deep, 1))
    assert((odd2(1.0)._1.toArray.deep, odd2(1.0)._2) == (Array(0, 1).deep, 2))
    assert((odd2(2.0)._1.toArray.deep, odd2(2.0)._2) == (Array(3, 4).deep, 2))
    assert((odd2(6.0)._1.toArray.deep, odd2(6.0)._2) == (Array(5).deep, 1))

    odd2.insert(1.0)
    odd2.insert(1.0)
    odd2.insert(0.0)

    assert((odd2(0.0)._1.toArray.deep, odd2(0.0)._2) == (Array(8).deep, 1))
    assert((odd2(1.0)._1.toArray.deep, odd2(1.0)._2) == (Array(6, 7).deep, 2))
    assert((odd2(2.0)._1.toArray.deep, odd2(2.0)._2) == (Array(3, 4).deep, 2))
    assert((odd2(6.0)._1.toArray.deep, odd2(6.0)._2) == (Array(5).deep, 1))

    odd2.insert(10.0)
    odd2.insert(-1.0)
    assert((odd2(0.0)._1.toArray.deep, odd2(0.0)._2) == (Array(8).deep, 1))
    assert((odd2(1.0)._1.toArray.deep, odd2(1.0)._2) == (Array(6, 7).deep, 2))
    assert(odd2.dindex.getOrElse(2.0, -1) == -1)
    assert((odd2(6.0)._1.toArray.deep, odd2(6.0)._2) == (Array(5).deep, 1))
    assert((odd2(10.0)._1.toArray.deep, odd2(10.0)._2) == (Array(9).deep, 1))
    assert((odd2(-1.0)._1.toArray.deep, odd2(-1.0)._2) == (Array(10).deep, 1))

    odd2.refresh

    assert((odd2(0.0)._1.toArray.deep, odd2(0.0)._2) == (Array(3).deep, 1))
    assert((odd2(1.0)._1.toArray.deep, odd2(1.0)._2) == (Array(1, 2).deep, 2))
    assert(odd2.dindex.getOrElse(2.0, -1) == -1)
    assert((odd2(6.0)._1.toArray.deep, odd2(6.0)._2) == (Array(0).deep, 1))
    assert((odd2(10.0)._1.toArray.deep, odd2(10.0)._2) == (Array(4).deep, 1))
    assert((odd2(-1.0)._1.toArray.deep, odd2(-1.0)._2) == (Array(5).deep, 1))
  }


}
