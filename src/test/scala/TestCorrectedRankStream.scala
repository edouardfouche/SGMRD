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

import io.github.edouardfouche.index.dimension.D_CRank_Stream
import org.scalatest.FunSuite

class TestCorrectedRankStream extends FunSuite {

  // init
  test(s"Check init for odd") {
    val odd1 = new D_CRank_Stream(Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0))
    assert(odd1(0) == (2, 1.0, 0.0, 0.0))
    assert(odd1(1) == (0, 2.0, 1.0, 0.0))
    assert(odd1(2) == (3, 3.0, 2.0, 0.0))
    assert(odd1(3) == (4, 4.0, 3.0, 0.0))
    assert(odd1(4) == (5, 5.0, 4.0, 0.0))
    assert(odd1(5) == (1, 6.0, 5.0, 0.0))
    assert(odd1(6) == (6, 7.0, 6.0, 0.0))
  }


  test(s"Check init for even") {
    val even1 = new D_CRank_Stream(Array(4.0, 2.0, 6.0, 1.0, 3.0, 5.0))
    assert(even1(0) == (3, 1.0, 0.0, 0.0))
    assert(even1(1) == (1, 2.0, 1.0, 0.0))
    assert(even1(2) == (4, 3.0, 2.0, 0.0))
    assert(even1(3) == (0, 4.0, 3.0, 0.0))
    assert(even1(4) == (5, 5.0, 4.0, 0.0))
    assert(even1(5) == (2, 6.0, 5.0, 0.0))
  }


  test(s"Check init for ties") {
    val ties1 = new D_CRank_Stream(Array(3.0, 2.0, 2.0, 1.0, 2.0))
    assert(ties1(0)._2 == 1.0)
    assert(ties1(1)._2 == 2.0)
    assert(ties1(2)._2 == 2.0)
    assert(ties1(3)._2 == 2.0)
    assert(ties1(4)._2 == 3.0)

    assert(ties1(0)._3 == 0.0)
    assert(ties1(1)._3 == 2.0)
    assert(ties1(2)._3 == 2.0)
    assert(ties1(3)._3 == 2.0)
    assert(ties1(4)._3 == 4.0)

    assert(ties1(0)._4 == 0.0)
    assert(ties1(1)._4 == 24.0)
    assert(ties1(2)._4 == 24.0)
    assert(ties1(3)._4 == 24.0)
    assert(ties1(4)._4 == 24.0)
  }

  val odd = Array(2.0, 6.0, 1.0, 3.0, 4.0, 5.0, 7.0)
  test(s"Check insert below delete for odd") {
    val odd2 = new D_CRank_Stream(odd)
    odd2.insert(1.5)
    assert(odd2(1) == (7, 1.5, -1.0, -1.0))
  }


  test(s"Check insert after delete for odd") {
    val odd3 = new D_CRank_Stream(odd)
    odd3.insert(2.5)
    assert(odd3(1) == (7, 2.5, -1, -1))
  }

  test(s"Check insert middle for odd") {
    val odd4 = new D_CRank_Stream(odd)
    odd4.insert(4.5)
    assert(odd4(3) == (7, 4.5, -1, -1))
  }


  test(s"Check insert begin for odd") {
    val odd5 = new D_CRank_Stream(odd)
    odd5.insert(0.5)
    assert(odd5(0) == (7, 0.5, -1, -1))
  }

  test(s"Check insert end for odd") {
    val odd6 = new D_CRank_Stream(odd)
    odd6.insert(7.5)
    assert(odd6(6) == (7, 7.5, -1, -1))
  }


  // insert, even
  val even = Array(4.0, 2.0, 6.0, 1.0, 3.0, 5.0)
  test(s"Check insert below delete for even") {

    val even2 = new D_CRank_Stream(even)
    even2.insert(3.5)
    assert(even2(3) == (6, 3.5, -1, -1))
  }

  test(s"Check insert after delete for even") {
    val even3 = new D_CRank_Stream(even)
    even3.insert(4.5)
    assert(even3(3) == (6, 4.5, -1, -1))
  }

  test(s"Check insert middle for even") {
    val even4 = new D_CRank_Stream(even)
    even4.insert(2.5)
    //println(even4.toString)
    assert(even4(2) == (6, 2.5, -1, -1))
  }

  test(s"Check insert begin for even") {
    val even5 = new D_CRank_Stream(even)
    even5.insert(0.5)
    assert(even5(0) == (6, 0.5, -1, -1))

  }

  test(s"Check insert end for even") {
    val even6 = new D_CRank_Stream(even)
    even6.insert(7.5)
    assert(even6(5) == (6, 7.5, -1, -1))
  }

  // insert, ties
  val tiesA = Array(3.0, 2.0, 2.0, 1.0, 2.0)
  test(s"A - Check insert below delete for ties") {
    val ties2 = new D_CRank_Stream(tiesA)
    ties2.insert(1.5)
    assert(ties2(1) == (5, 1.5, -1, -1))
  }

  test(s"A - Check insert after delete for ties") {
    val ties3 = new D_CRank_Stream(tiesA)
    ties3.insert(2.5)
    assert(ties3(4) == (5, 2.5, -1, -1))
  }

  test(s"A - Check insert middle for ties") {
    val ties4 = new D_CRank_Stream(tiesA)
    ties4.insert(2.0)
    //assert(ties4(0) == (3, 1.0, 0.0, 0.0))
    //assert(ties4(1) == (1, 2.0, 2.0, 24))
    //assert(ties4(2) == (2, 2.0, 2.0, 24))
    //assert(ties4(3) == (4, 2.0, 2.0, 24))
    //assert(ties4(4) == (5, 2.0, -1, -1))
    assert(ties4(0)._2 == 1.0)
    assert(ties4(1)._2 == 2.0)
    assert(ties4(2)._2 == 2.0)
    assert(ties4(3)._2 == 2.0)
    assert(ties4(4)._2 == 2.0)
    ties4.refresh
    assert(ties4(0)._2 == 1.0)
    assert(ties4(1)._2 == 2.0)
    assert(ties4(2)._2 == 2.0)
    assert(ties4(3)._2 == 2.0)
    assert(ties4(4)._2 == 2.0)

    assert(ties4(0)._3 == 0.0)
    assert(ties4(1)._3 == 2.5)
    assert(ties4(2)._3 == 2.5)
    assert(ties4(3)._3 == 2.5)
    assert(ties4(4)._3 == 2.5)

    assert(ties4(0)._4 == 0.0)
    assert(ties4(1)._4 == 60)
    assert(ties4(2)._4 == 60)
    assert(ties4(3)._4 == 60)
    assert(ties4(4)._4 == 60)
  }

  val tiesB = Array(3.0, 2.0, 2.0, 4.0, 2.0)
  test(s"B - Check insert below delete for ties") {
    val ties5 = new D_CRank_Stream(tiesB)
    ties5.insert(1.5)
    assert(ties5(0) == (5, 1.5, -1, -1))
  }

  test(s"B - Check insert after delete for ties") {
    val ties6 = new D_CRank_Stream(tiesB)
    ties6.insert(2.5)
    assert(ties6(3) == (5, 2.5, -1, -1))
  }


  test(s"B - Check insert middle for ties") {
    val ties7 = new D_CRank_Stream(tiesB)
    ties7.insert(2.0)
    assert(ties7(0)._2 == 2.0)
    assert(ties7(1)._2 == 2.0)
    assert(ties7(2)._2 == 2.0)
    assert(ties7(3)._2 == 2.0)
  }

  val tiesC = Array(0.0, 2.0, 2.0, 1.0, 2.0)
  test(s"C - Check insert below delete for ties") {

    val ties8 = new D_CRank_Stream(tiesC)
    ties8.insert(1.5)
    assert(ties8(1) == (5, 1.5, -1, -1))
  }


  test(s"C - Check insert after delete for ties") {
    val ties9 = new D_CRank_Stream(tiesC)
    ties9.insert(2.5)
    assert(ties9(4) == (5, 2.5, -1, -1))
  }


  test(s"C - Check insert middle for ties") {
    val ties10 = new D_CRank_Stream(tiesC)
    ties10.insert(2.0)
    assert(ties10(1)._2 == 2.0)
    assert(ties10(2)._2 == 2.0)
    assert(ties10(3)._2 == 2.0)
    assert(ties10(4)._2 == 2.0)
  }

  val tiesD = Array(2.0, 0.0, 2.0, 1.0, 2.0)
  test(s"D - Check insert below delete for ties") {

    val ties8 = new D_CRank_Stream(tiesD)
    ties8.insert(1.5)
    assert(ties8(2) == (5, 1.5, -1, -1))
  }


  test(s"D - Check insert after delete for ties") {
    val ties9 = new D_CRank_Stream(tiesD)
    ties9.insert(2.5)
    assert(ties9(4) == (5, 2.5, -1, -1))
  }


  test(s"D - Check insert middle for ties") {
    val ties10 = new D_CRank_Stream(tiesD)
    //println(ties10)
    ties10.insert(2.0)
    //println(ties10)
    assert(ties10(1)._2 == 1.0)
    assert(ties10(2)._2 == 2.0)
    assert(ties10(3)._2 == 2.0)
    assert(ties10(4)._2 == 2.0)
  }

  val tiesE = Array(2.0, 3.0, 2.0, 1.0, 2.0)
  test(s"E - Check insert below delete for ties") {
    val ties8 = new D_CRank_Stream(tiesE)
    ties8.insert(1.5)
    assert(ties8(1) == (5, 1.5, -1, -1))
  }


  test(s"E - Check insert after delete for ties") {
    val ties9 = new D_CRank_Stream(tiesE)
    ties9.insert(2.5)
    assert(ties9(3) == (5, 2.5, -1, -1))
  }


  test(s"E - Check insert middle for ties") {
    val ties10 = new D_CRank_Stream(tiesE)
    //println(ties10)
    ties10.insert(2.0)
    //println(ties10)
    assert(ties10(1)._2 == 2.0)
    assert(ties10(2)._2 == 2.0)
    assert(ties10(3)._2 == 2.0)
    assert(ties10(4)._2 == 3.0)
  }

  val tiesF = Array(2.0, 3.0, 2.0, 4.0, 2.0)
  test(s"F - Check insert below delete for ties") {
    val ties8 = new D_CRank_Stream(tiesF)
    ties8.insert(1.5)
    assert(ties8(0) == (5, 1.5, -1, -1))
  }


  test(s"F - Check insert after delete for ties") {
    val ties9 = new D_CRank_Stream(tiesF)
    ties9.insert(2.5)
    assert(ties9(2) == (5, 2.5, -1, -1))
  }


  test(s"F - Check insert middle for ties") {
    val ties10 = new D_CRank_Stream(tiesF)
    ties10.insert(2.0)
    assert(ties10(0)._2 == 2.0)
    assert(ties10(1)._2 == 2.0)
    assert(ties10(2)._2 == 2.0)
    assert(ties10(3)._2 == 3.0)
  }

  test(s"Test Scenario 1") {
    val odd1 = new D_CRank_Stream(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
    odd1.insert(1.0)
    assert(odd1(0) == (5, 0.0, 0.0, 0.0))
    assert(odd1(1)._2 == 1.0)
    assert(odd1(2)._2 == 1.0)
    assert(odd1(3)._2 == 1.0)
    assert(odd1(4) == (1, 2.0, 3.5, 12.0))

    odd1.insert(1.0)
    assert(odd1(0) == (5, 0.0, 0.0, 0.0))
    assert(odd1(1)._2 == 1.0)
    assert(odd1(2)._2 == 1.0)
    assert(odd1(3)._2 == 1.0)
    assert(odd1(4)._2 == 1.0)
    assert(odd1(5) == (2, 6.0, 5.0, 12.0))

    odd1.insert(6.0)
    assert(odd1(0) == (5, 0.0, 0.0, 0.0))
    assert(odd1(1)._2 == 1.0)
    assert(odd1(2)._2 == 1.0)
    assert(odd1(3)._2 == 1.0)
    assert(odd1(4)._2 == 1.0)
    assert(odd1(5) == (8, 6.0, -1, -1))

    odd1.refresh
    assert(odd1(0)._2 == 0.0)
    assert(odd1(1)._2 == 1.0)
    assert(odd1(2)._2 == 1.0)
    assert(odd1(3)._2 == 1.0)
    assert(odd1(4)._2 == 1.0)
    assert(odd1(5)._2 == 6.0)

    assert(odd1(0)._3 == 0.0)
    assert(odd1(1)._3 == 2.5)
    assert(odd1(2)._3 == 2.5)
    assert(odd1(3)._3 == 2.5)
    assert(odd1(4)._3 == 2.5)
    assert(odd1(5)._3 == 5.0)
  }

  test("Test Scenario 2") {
    val odd2 = new D_CRank_Stream(Array(2.0, 2.0, 6.0, 1.0, 1.0, 0.0))
    odd2.insert(2.0)
    odd2.insert(2.0)
    odd2.insert(6.0)
    assert(odd2(0) == (5, 0.0, 0.0, 0.0))
    assert(odd2(1)._2 == 1.0)
    assert(odd2(2)._2 == 1.0)
    assert(odd2(3)._2 == 2.0)
    assert(odd2(4)._2 == 2.0)
    assert(odd2(5) == (8, 6.0, -1, -1))

    odd2.refresh
    assert(odd2(0) == (2, 0.0, 0.0, 0.0))
    assert(odd2(1)._2 == 1.0)
    assert(odd2(2)._2 == 1.0)
    assert(odd2(3)._2 == 2.0)
    assert(odd2(4)._2 == 2.0)
    assert(odd2(5) == (5, 6.0, 5.0, 12))

    odd2.insert(1.0)
    odd2.insert(1.0)
    odd2.insert(0.0)

    assert(odd2(0) == (8, 0.0, -1, -1))
    assert(odd2(1)._2 == 1.0)
    assert(odd2(2)._2 == 1.0)
    assert(odd2(3)._2 == 2.0)
    assert(odd2(4)._2 == 2.0)
    assert(odd2(5) == (5, 6.0, 5, 12))

    odd2.insert(10.0)
    odd2.insert(-1.0)
    assert(odd2(0) == (10, -1.0, -1, -1))
    assert(odd2(5) == (9, 10.0, -1, -1))

    odd2.refresh

    assert(odd2(0) == (5, -1.0, 0.0, 0.0))
    assert(odd2(1) == (3, 0.0, 1.0, 0.0))
    assert(odd2(2)._2 == 1.0)
    assert(odd2(3)._2 == 1.0)
    assert(odd2(4) == (0, 6.0, 4, 6))
    assert(odd2(5) == (4, 10.0, 5, 6))

    assert(odd2(2)._3 == 2.5)
    assert(odd2(3)._3 == 2.5)
  }


}
