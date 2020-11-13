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

/**
  * The index for an ordinal dimension, or a numerical dimension with ties (intended for MWP)
  * Compute an adjusted, corrected rank index from a given data set
  * The rank are adjusted, which means that in the case of ties, the rank is defined as the average rank of the tying values
  * Also, a "correction for ties" is computed, as required to compute a Mann-Whitney U test
  * The correction is computed as a cumulative value.
  *
  * @param initvalues An array of values corresponding to the values in a column
  */
class D_CRank(val initvalues: Array[Double]) extends DimensionIndex {
  type T = (Int, Double, Float, Double)
  //first element (Int) -> position
  //second element (Double) -> value
  //third element (Float) -> adjustedrank
  //fourth element (Double) -> correction

  val id = "CRank"
  var currentvalues = initvalues.toVector

  var dindex: Array[T] = createDimensionIndex(initvalues.toVector)

  def apply(n: Int): T = dindex(n) // access in the index

  override def toString: String = dindex mkString ";"

  def refresh(): Unit = {}

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    currentvalues = currentvalues.drop(1) :+ newpoint
    dindex = createDimensionIndex(currentvalues)
  }

  def createDimensionIndex(input: Vector[Double]): Array[T] = {
    // Create an index for each column with this shape: (original position, adjusted rank, original value)
    // They are ordered by rank
    val nonadjusted = {
      //input.zipWithIndex.sortBy(_._1).zipWithIndex.map(y => (y._1._2, y._1._1, y._2.toFloat, y._1._1))
      // yields to java.lang.IllegalArgumentException: Comparison method violates its general contract
      //input.zipWithIndex.sortWith((x,y) => (x._1 < y._1) || ((x._1 == y._1) && math.random < 0.5)).zipWithIndex.map(y => (y._1._2, y._1._1, y._2.toFloat, y._1._1))
      // Note: the code above breaks when there are NaN
      val rd = scala.util.Random.shuffle(input.indices.toList) // tie breaking random list
      input.zipWithIndex.zip(rd).map(x => (x._1._1, x._1._2, x._2)).
        sortWith((x, y) => (x._1 < y._1) || ((x._1 == y._1) && x._3 < y._3)).
        zipWithIndex.map(y => (y._1._2, y._1._1, y._2.toFloat, y._1._1))
    }
    val adjusted = new Array[T](input.length)

    val m = nonadjusted.length - 1
    var j = 0
    var acc_corr = 0.0
    while (j <= m) {
      var k = j
      var acc = 0.0
      // && is quite important here, as if the first condition is false you don't want to evaluate the second
      while ((k < m) && (nonadjusted(k)._2 == nonadjusted(k + 1)._2)) { // Wooo we are comparing doubles here, is that ok? I guess yes
        acc += nonadjusted(k)._3
        k += 1
      }
      if (k > j) {
        val newval = ((acc + nonadjusted(k)._3) / (k - j + 1.0)).toFloat
        val t = k - j + 1.0
        acc_corr = acc_corr + math.pow(t, 3) - t
        (j to k).foreach(y => adjusted(y) = (nonadjusted(y)._1, nonadjusted(y)._2, newval, acc_corr))
        j += k - j + 1 // jump to after the replacement
      } else {
        adjusted(j) = (nonadjusted(j)._1, nonadjusted(j)._2, nonadjusted(j)._3, acc_corr)
        j += 1
      }
    }
    adjusted
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    // Performing safe cut
    val start = scala.util.Random.nextInt((length - sliceSize).max(1)) //+1)
    val sliceStart = getSafeCut(start)
    val sliceEndSearchStart = (sliceStart + sliceSize).min(length - 1)
    val sliceEnd = getSafeCut(sliceEndSearchStart)

    for {x <- 0 until sliceStart} {
      logicalArray(dindex(x)._1) = false
    }
    for {x <- sliceEnd until dindex.length} {
      logicalArray(dindex(x)._1) = false
    }

    // Correcting the slice size
    val currentsliceSize = sliceEnd - sliceStart
    if (currentsliceSize > sliceSize) { // then release some
      val torelease = scala.util.Random.shuffle((sliceStart until sliceEnd).toList).take(currentsliceSize - sliceSize)
      torelease.foreach(x => logicalArray(dindex(x)._1) = false)
    } else if (currentsliceSize < sliceSize) { // then reset some to true // TODO: Seems somewhat suboptimal
      val toreset = scala.util.Random.shuffle((0 until sliceStart).toList ::: (sliceEnd until dindex.length).toList).take(sliceSize - currentsliceSize)
      toreset.foreach(x => logicalArray(dindex(x)._1) = true)
    }

    logicalArray
  }

  // This kind of slicing aim to alleviate the "border effect". Every point are equally likely to be selected
  override def uniformslice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)

    val start = scala.util.Random.nextInt(length) //+1)
    val sliceStart = getSafeCut(start)
    val sliceEndSearchStart = (sliceStart + sliceSize) % length
    val sliceEnd = getSafeCut(sliceEndSearchStart)

    if (sliceStart <= sliceEnd) {
      for {x <- 0 until sliceStart} {
        logicalArray(dindex(x)._1) = false
      }
      for {x <- sliceEnd until dindex.length} {
        logicalArray(dindex(x)._1) = false
      }
    } else {
      for {x <- 0 until sliceEnd} {
        logicalArray(dindex(x)._1) = false
      }
      for {x <- sliceStart until dindex.length} {
        logicalArray(dindex(x)._1) = false
      }
    }

    // Correcting the slice size
    val currentsliceSize = sliceEnd - sliceStart
    if (currentsliceSize > sliceSize) { // then release some
      val torelease = scala.util.Random.shuffle((sliceStart until sliceEnd).toList).take(currentsliceSize - sliceSize)
      torelease.foreach(x => logicalArray(dindex(x)._1) = false)
    } else if (currentsliceSize < sliceSize) { // then reset some to true
      val toreset = scala.util.Random.shuffle((0 until sliceStart).toList ::: (sliceEnd until dindex.length).toList).take(sliceSize - currentsliceSize)
      toreset.foreach(x => logicalArray(dindex(x)._1) = true)
    }

    logicalArray
  }

  // Helper function to avoid cutting between ties
  def getSafeCut(cut: Int): Int = {
    //require(cut >= 0 & cut <= reference.length)
    //val ref = index(reference)
    //println(s"ref.length: ${ref.length}: ref($cut): ${ref(cut)} : ref(${cut+1}): ${ref(cut+1)}")
    @tailrec def cutSearch(a: Int, inc: Int = 0, ref: D_CRank): Int = {
      // "It's easier to ask forgiveness than it is to get permission"
      try if (ref(a + inc)._2 != ref(a + inc - 1)._2) return a + inc
      else {
        try if (ref(a - inc)._2 != ref(a - inc - 1)._2) return a - inc
        catch {
          case _: Throwable => return a - inc
        }
      }
      catch {
        case _: Throwable => return a + inc
      }
      cutSearch(a, inc + 1, ref)
    }

    cutSearch(cut, 0, this)
  }

  // We break the precedent function into two alternatives: getSafeCutRight and getSafeCutLeft
  def getSafeCutRight(cut: Int): Int = {
    var i = 0
    while ((cut + i + 1) < this.length && this (cut)._2 == this (cut + i + 1)._2) {
      i = i + 1
    }
    cut + i + 1 // be careful with this plus one, it is because we return
  }

  def getSafeCutLeft(cut: Int): Int = {
    var i = 0
    while ((cut + i - 1) >= 0 && this (cut)._2 == this (cut + i - 1)._2) {
      i = i - 1
    }
    cut + i
  }
}
