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

/**
  * The index for a numerical dimension (intended for KSPemr)
  *
  * @param initvalues An array of values corresponding to the values in a column
  */
class D_Rank(val initvalues: Array[Double]) extends DimensionIndex {
  type T = (Int, Double) // T_Rank
  val id = "Rank"
  // first element (Int) -> position
  // second elements (Double) -> value
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
    //input.zipWithIndex.sortBy(_._1).map(x => (x._2, x._1))
    // tie breaking random list
    // I had the idea for this trick from here: https://stackoverflow.com/questions/44440018/handling-scala-array-group-with-ties
    // Note: the code above breaks when there are NaN
    val rd = scala.util.Random.shuffle(input.indices.toList) // tie breaking random list
    input.zipWithIndex.zip(rd).map(x => (x._1._1, x._1._2, x._2)).
      sortWith((x, y) => (x._1 < y._1) || ((x._1 == y._1) && x._3 < y._3)).map(x => (x._2, x._1)).toArray
  }

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)
    val sliceStart = scala.util.Random.nextInt((length - sliceSize).max(1))
    for {x <- 0 until sliceStart} {
      logicalArray(dindex(x)._1) = false
    }
    for {x <- sliceStart + sliceSize until dindex.length} {
      logicalArray(dindex(x)._1) = false
    }
    logicalArray
  }

  def uniformslice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(true)

    val sliceStart = scala.util.Random.nextInt(length) //+1)
    val sliceEnd = (sliceStart + sliceSize) % length

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

    logicalArray
  }
}
