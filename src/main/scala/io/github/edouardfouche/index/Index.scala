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
package io.github.edouardfouche.index

import io.github.edouardfouche.index.dimension.DimensionIndex
import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport

abstract class Index[+T <: DimensionIndex] {
  val data: DataSet
  val parallelize: Int
  val id: String
  val index: Vector[T] = createIndex(data)

  // Convert to the same index but with stream operations.
  def toStream(): Index[T]

  def insert(newpoints: Array[Double]): Unit = {
    require(data.ncols == newpoints.length)
    if (parallelize == 0) {
      (0 until data.ncols).foreach { x =>
        index(x).insert(newpoints(x))
      }
    } else {
      val columns = (0 until data.ncols).par
      if (parallelize > 1) columns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      columns.foreach { x =>
        index(x).insert(newpoints(x))
      }
    }
  }

  def refresh(): Unit = {
    if (parallelize == 0) {
      index.foreach(x => x.refresh())
    } else {
      val parindex = index.par
      if (parallelize > 1) parindex.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      parindex.foreach(x => x.refresh())
    }
  }

  def apply(n: Int): T = index(n) // access the columns of the index

  def indices: Range = 0 until data.ncols // this is supposed to give the indices of the columns

  def ncols: Int = data.ncols

  def nrows: Int = data.nrows

  def isEmpty: Boolean = data.ncols == 0

  /**
    * Produce a subspace slice by conditioning on all dimensions, except a reference dimension
    *
    * @param dimensions   The set of dimensions of the subspaces
    * @param referenceDim The dimension that is considered as reference
    * @param sliceSize    The size of the slice for each dimensions, determined by alpha
    * @return Returns an array of booleans. True corresponds to indexes included in the slice.
    */
  def randomSlice(dimensions: Set[Int], referenceDim: Int, sliceSize: Int): Array[Boolean] = {
    dimensions.filter(_ != referenceDim).map(x => index(x).slice(sliceSize)).toArray.transpose.map(x => !x.contains(false))
  }

  /**
    * Produce a subspace slice by conditioning on all dimensions, except a reference dimension
    *
    * @param dimensions   The set of dimensions of the subspaces
    * @param referenceDim The dimension that is considered as reference
    * @param sliceSize    The size of the slice for each dimensions, determined by alpha
    * @return Returns an array of booleans. True corresponds to indexes included in the slice.
    */
  def uniformSlice(dimensions: Set[Int], referenceDim: Int, sliceSize: Int): Array[Boolean] = {
    dimensions.filter(_ != referenceDim).map(x => index(x).uniformslice(sliceSize)).toArray.transpose.map(x => !x.contains(false))
  }

  /**
    * Find the greatest common divisor of a and b
    *
    * @param a integer
    * @param b integer
    * @return An integer, the greatest common divisor of a and b
    */
  @tailrec
  final def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  /**
    * Coprimality test between two integers a and b
    *
    * @param a integer
    * @param b integer
    * @return boolean, true if a and b are coprimes
    */
  def areCoPrimes(a: Int, b: Int): Boolean = gcd(a, b) == 1

  /**
    * Helper function that generates a stream of integers that are close to a, shall not be smaller than 1
    *
    * @param a   starting integer
    * @param inc increment (internal parameter with default value 1)
    * @return A stream of integer close to a and greater or equal to 1
    */
  def closeSearch(a: Int, inc: Int = 1): Stream[Int] = {
    if (a - inc > 1) (a + inc) #:: (a - inc) #:: closeSearch(a, inc + 1)
    else (a + inc) #:: closeSearch(a, inc + 1)
  }

  /**
    * Find the closest integer from a, which is coprime with ref
    *
    * @param a   An integer. We want to find the closest coprime from this position
    * @param ref A reference integer.
    * @return The cloest integer from a which is coprime with ref
    */
  def findClosestCoPrime(a: Int, ref: Int): Int = {
    if (areCoPrimes(a, ref)) a
    else closeSearch(a).filter(areCoPrimes(_, ref)).head
  }

  /**
    * Initialize the index
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  protected def createIndex(data: DataSet): Vector[T]
}
