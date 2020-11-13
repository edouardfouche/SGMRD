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
package io.github.edouardfouche.index.dimension

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * The index for an ordinal dimension, or a numerical dimension with ties, with stream operations (intended for MWP)
  * Compute an adjusted, corrected rank index from a given data set
  * The rank are adjusted, which means that in the case of ties, the rank is defined as the average rank of the tying values
  * Also, a "correction for ties" is computed, as required to compute a Mann-Whitney U test
  * The correction is computed as a cumulative value.
  *
  * @param initvalues An array of values corresponding to the values in a column
  */
class D_CRank_Stream(initvalues: Array[Double]) extends D_CRank(initvalues) with DimensionIndexStream {
  //Remember the structure of the index:
  //first element (Int) -> position
  //second element (Double) -> value
  //third element (Float) -> adjustedrank
  //fourth element (Double) -> correction

  override val id = "CRankStream"
  val queue: mutable.Queue[Double] = scala.collection.mutable.Queue[Double](initvalues: _*)
  var offset: Int = 0

  override def refresh: Unit = {
    if (offset > 0) {
      dindex = dindex.zipWithIndex.map(x => (x._1._1 - offset, x._1._2, x._2.toFloat, x._1._2))

      val m = dindex.length - 1
      var j = 0
      var acc_corr = 0.0
      while (j <= m) {
        var k = j
        var acc = 0.0
        // && is quite important here, as if the first condition is false you don't want to evaluate the second
        while ((k < m) && (dindex(k)._2 == dindex(k + 1)._2)) { // Wooo we are comparing doubles here, is that ok? I guess yes
          acc += dindex(k)._3
          k += 1
        }
        if (k > j) {
          val newval = ((acc + dindex(k)._3) / (k - j + 1.0)).toFloat
          val t = k - j + 1.0
          acc_corr = acc_corr + math.pow(t, 3) - t
          (j to k).foreach(y => dindex(y) = (dindex(y)._1, dindex(y)._2, newval, acc_corr))
          j += k - j + 1 // jump to after the replacement
        } else {
          dindex(j) = (dindex(j)._1, dindex(j)._2, dindex(j)._3, acc_corr)
          j += 1
        }
      }
      offset = 0
    }

  }

  // Remark: I noticed that the insertion is quite slow in case the space is discrete (randomize in some other way?)
  override def insert(newpoint: Double): Unit = {
    // currentvalues = currentvalues.drop(1) :+ newpoint // we don't need it for MWP stream
    val todelete = queue.dequeue

    def binarySearch(start: Int, end: Int, value: Double): Int = {
      @tailrec
      def binarySearch_acc(start: Int, end: Int, value: Double): Int = {
        val i = (end + start) / 2
        //println(s"start: $start, end: $end, i:$i")
        (dindex(i), dindex(i + 1)) match {
          case (x, y) if (x._2 == value) && (y._2 == value) =>
            var oldest = if (x._1 < y._1) i else i + 1
            var j = oldest - 1
            var k = oldest + 1
            while ((j >= 0) && (dindex(j)._2 == value)) {
              if (dindex(j)._1 < dindex(oldest)._1) oldest = j
              j -= 1
            }
            while ((k < dindex.length) && (dindex(k)._2 == value)) {
              if (dindex(k)._1 < dindex(oldest)._1) oldest = k
              k += 1
            }
            oldest
          case (x, y) if (x._2 == value) =>
            var oldest = i
            var j = oldest - 1
            while ((j >= 0) && (dindex(j)._2 == value)) {
              if (dindex(j)._1 < dindex(oldest)._1) oldest = j
              j -= 1
            }
            oldest
          case (x, y) if (y._2 == value) =>
            var oldest = i + 1
            var k = oldest + 1
            while ((k < dindex.length) && (dindex(k)._2 == value)) {
              if (dindex(k)._1 < dindex(oldest)._1) oldest = k
              k += 1
            }
            oldest
          case (x, y) if x._2 > value => binarySearch_acc(start, i - 1, value)
          case (x, y) if y._2 < value => binarySearch_acc(i + 1, end, value)
        }
      }

      if (dindex(0)._2 == value) {
        var oldest = 0
        var k = oldest + 1
        while ((k < dindex.length) && (dindex(k)._2 == value)) {
          if (dindex(k)._1 < dindex(oldest)._1) oldest = k
          k += 1
        }
        oldest
      }
      else if (dindex(dindex.length - 1)._2 == value) {
        var oldest = dindex.length - 1
        var j = oldest - 1
        while ((j >= 0) && (dindex(j)._2 == value)) {
          if (dindex(j)._1 < dindex(oldest)._1) oldest = j
          j -= 1
        }
        oldest
      }
      else binarySearch_acc(0, dindex.length - 1, value)
    }

    // the binary search returns the index on the match with oldest position
    // if no match, the index just after
    def binarySearch_insert(start: Int, end: Int, value: Double): Int = {
      @tailrec
      def binarySearch_acc(start: Int, end: Int, value: Double): Int = { // this binary search is good only for finding the point where to insert
        val i = (end + start) / 2
        //println(s"start: $start, end: $end, i:$i")
        (dindex(i)._2, dindex(i + 1)._2) match {
          case (x, y) if (x <= value) & (y >= value) => i + 1
            var possibleinsert = mutable.MutableList(i + 1)
            var j = i
            var k = i + 2
            while ((j >= 0) && (dindex(j)._2 == value)) {
              possibleinsert = possibleinsert :+ j
              j -= 1
            }
            while ((k < dindex.length) && ((dindex(k - 1)._2 <= value) & (dindex(k)._2 >= value))) {
              possibleinsert = possibleinsert :+ k
              k += 1
            }
            scala.util.Random.shuffle(possibleinsert).head

          case x if x._1 >= value => binarySearch_acc(start, i - 1, value)
          case x if x._2 < value => binarySearch_acc(i + 1, end, value)
        }
      }

      if (dindex(0)._2 > value) 0
      else if (dindex(dindex.length - 1)._2 < value) dindex.length
      else binarySearch_acc(0, dindex.length - 1, value)
    }

    val indextodelete = binarySearch(0, dindex.length - 1, todelete) // will always be pointing to an object (the oldest one with value = todelete)
    val indextoinsert = if (newpoint == todelete) indextodelete + 1 else binarySearch_insert(0, dindex.length - 1, newpoint) // will pointing between two objects, i.e, the one after.

    if ((indextoinsert == (indextodelete + 1)) || (indextoinsert == indextodelete)) { // in that case it simply replaces the point
      dindex(indextodelete) = (dindex.length + offset, newpoint, -1, -1)
    } else {
      if (indextoinsert == 0) {
        for (x <- (indextoinsert + 1 to indextodelete).reverse) {
          dindex(x) = dindex(x - 1)
        }
        dindex(indextoinsert) = (dindex.length + offset, newpoint, -1, -1)
      } else if (indextoinsert == dindex.length) {
        for (x <- indextodelete until indextoinsert - 1) {
          dindex(x) = dindex(x + 1)
        }
        dindex(indextoinsert - 1) = (dindex.length + offset, newpoint, -1, -1)
      } else if (indextoinsert < indextodelete) {
        for (x <- (indextoinsert to indextodelete).reverse) {
          dindex(x) = dindex(x - 1)
        }
        dindex(indextoinsert) = (dindex.length + offset, newpoint, -1, -1)
      } else if (indextoinsert > indextodelete) {
        for (x <- indextodelete until indextoinsert) {
          dindex(x) = dindex(x + 1)
        }
        dindex(indextoinsert - 1) = (dindex.length + offset, newpoint, -1, -1)
      }
    }
    offset += 1
    queue += newpoint
    //assert(dindex.sortBy(_._2).deep == dindex.deep, s"Sorting broken in this round: \n ${dindex.mkString(",")}")
  }
}
