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
package io.github.edouardfouche.mcde

import io.github.edouardfouche.index.Index
import io.github.edouardfouche.index.dimension.DimensionIndex
import io.github.edouardfouche.preprocess.DataSet

import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by edouardfouche on 07.07.17.
  */
trait McdeStats extends Stats {
  type D <: DimensionIndex
  type I <: Index[D]
  val id: String
  val alpha: Double
  val beta: Double // Added to loose the dependence of beta from alpha
  val M: Int
  var parallelize: Int

  def getIndexConstruct: DataSet => I

  def getDIndexConstruct: Array[Double] => D

  require(alpha > 0 & alpha < 1, "alpha should be greater than 0 and lower than 1")
  require(M > 0, "M should be greater than 0")
  require(beta > 0 & beta <= 1, "beta should be greater than 0 and lower than 1")

  def preprocess(input: DataSet, stream: Boolean = false): I

  def preprocess(input: Array[Array[Double]], stream: Boolean): I = { // , stream: Boolean = false
    preprocess(new DataSet(input), stream)
  }

  /**
    * Statistical test computation
    *
    * @param indexSelection An array of Boolean that contains the information if a given index is in the slice
    */
  //@param reference      The vector of the reference dimension as an array of 2-Tuple. First element is the index, the second is the rank
  def twoSample(index: D, indexSelection: Array[Boolean]): Double

  override def contrast(m: DataSet, dimensions: Set[Int]): Double = {
    this.contrast(this.preprocess(m), dimensions)
  }

  /**
    * Compute the contrast of a subspace
    *
    * @param m          The indexes from the original data ordered by the rank of the points
    * @param dimensions The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @return The contrast of the subspace (value between 0 and 1)
    */
  def contrast(m: I, dimensions: Set[Int]): Double = {
    // Sanity check
    m.refresh()
    require(dimensions.forall(x => x >= 0 & x < m.ncols), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.nrows).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = if (parallelize == 0) {
      (1 to M).map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m(referenceDim), m.randomSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    } else {
      val iterations = (1 to M).par
      if (parallelize > 1) {
        //iterations.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m(referenceDim), m.randomSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    }

    result
  }

  /**
    * Compute the contrast of a subspace // This is a version where alpha is always choosen at random between 0.1 and 0.9
    *
    * @param m          The indexes from the original data ordered by the rank of the points
    * @param dimensions The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @return The contrast of the subspace (value between 0 and 1)
    */
  def contrastAlpha(m: I, dimensions: Set[Int]): Double = {
    // Sanity check
    m.refresh()
    require(dimensions.forall(x => x >= 0 & x < m.ncols), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")

    val result = if (parallelize == 0) {
      (1 to M).map(i => {
        val alpha = (scala.util.Random.nextInt(9) + 1) / 10.0
        val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.nrows).ceil.toInt /// WARNING: Do not forget -1
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m(referenceDim), m.randomSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    } else {
      val iterations = (1 to M).par
      if (parallelize > 1) {
        //iterations.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val alpha = (scala.util.Random.nextInt(9) + 1) / 10.0
        val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.nrows).ceil.toInt /// WARNING: Do not forget -1
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m(referenceDim), m.randomSlice(dimensions, referenceDim, sliceSize))
      }).sum / M
    }

    result
  }

  /**
    * Compute the deviation of a subspace with respect to a particular dimension // This is a version where alpha is always choosen at random between 0.1 and 0.9
    *
    * @param m            The indexes from the original data ordered by the rank of the points
    * @param dimensions   The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @param referenceDim The reference dimensions, should be contained in dimensions
    * @return A 2-D Array contains the contrast for each pairwise dimension
    */
  def deviationAlpha(m: I, dimensions: Set[Int], referenceDim: Int): Double = {
    // Sanity check
    m.refresh()
    require(dimensions.forall(x => x >= 0 & x < m.ncols), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    require(dimensions.contains(referenceDim), "The reference dimensions should be contained in the set of dimensions")

    val result = (1 to M).map(i => {
      val alpha = (scala.util.Random.nextInt(9) + 1) / 10.0
      val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.nrows).ceil.toInt /// WARNING: Do not forget -1
      twoSample(m(referenceDim), m.randomSlice(dimensions, referenceDim, sliceSize))
    }).sum / M //, targetSampleSize))).sum / M

    result
  }

  def deviation(m: DataSet, dimensions: Set[Int], referenceDim: Int): Double = {
    this.deviation(this.preprocess(m), dimensions, referenceDim)
  }

  /**
    * Compute the pairwise contrast matrix for a given data set
    * Note: This matrix is symmetric
    *
    * @param m The indexes from the original data ordered by the rank of the points
    * @return A 2-D Array contains the contrast for each pairwise dimension
    */
  def contrastMatrix(m: I): Array[Array[Double]] = {
    m.refresh()

    val numCols = m.ncols
    val matrix = Array.ofDim[Double](numCols, numCols)

    val cols = if (parallelize == 0) {
      0 until numCols
    } else {
      val colspar = (0 until numCols).par
      if (parallelize > 1) {
        //colspar.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        colspar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      colspar
    }

    // TODO: The problem here is that the underlying stat is also parallelized, when parallelism is activated.
    // The current workaround is to have parallize as a variable and set it temporarily to 0 (but that is not very good)
    val currentparallelismlevel = parallelize
    parallelize = 0

    for {
      x <- cols
      y <- 0 until x
    } yield {
      val c = contrast(m, Set(x, y))
      matrix(x)(y) = c
      matrix(y)(x) = c
    }

    parallelize = currentparallelismlevel
    matrix
  }

  def deviationMatrix(m: DataSet): Array[Array[Double]] = {
    deviationMatrix(preprocess(m))
  }

  /**
    * Compute the pairwise deviation matrix for a given data set
    * Note: This matrix is asymmetric
    *
    * @param m The indexes from the original data ordered by the rank of the points
    * @return A 2-D Array contains the deviation for each pairwise dimension
    */
  def deviationMatrix(m: I): Array[Array[Double]] = {
    // Sanity check
    m.refresh()
    //require(alpha > 0 & alpha < 1, "alpha should be greater than 0 and lower than 1")
    //require(M > 0, "M should be greater than 0")
    val numCols = m.ncols
    val matrix = Array.ofDim[Double](numCols, numCols)

    val cols = if (parallelize == 0) {
      0 until numCols
    } else {
      val colspar = (0 until numCols).par
      if (parallelize > 1) {
        //colspar.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        colspar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      colspar
    }

    for {
      x <- cols //0 until numCols//.par // let's try not to parallelize here
      y <- 0 until x
    } yield {
      matrix(x)(y) = deviation(m, Set(x, y), x)
      matrix(y)(x) = deviation(m, Set(x, y), y)
    }
    matrix
  }

  /**
    * Compute the deviation of a subspace with respect to a particular dimension
    *
    * @param m            The indexes from the original data ordered by the rank of the points
    * @param dimensions   The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @param referenceDim The reference dimensions, should be contained in dimensions
    * @return A 2-D Array contains the contrast for each pairwise dimension
    */
  def deviation(m: I, dimensions: Set[Int], referenceDim: Int): Double = {
    // Sanity check

    m.refresh()
    require(dimensions.forall(x => x >= 0 & x < m.ncols), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    require(dimensions.contains(referenceDim), "The reference dimensions should be contained in the set of dimensions")

    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m(0).length).ceil.toInt /// WARNING: Do not forget -1


    //val indexreference = m.randomSlice(dimensions, referenceDim, sliceSize)
    //println(s"indexreference.length: ${indexreference.length} dimensions: ${dimensions}, referenceDim: ${referenceDim}, sliceSize: ${sliceSize}")
    val result = (1 to M).map { i =>
      val indexreference = m.randomSlice(dimensions, referenceDim, sliceSize)
      twoSample(m(referenceDim), indexreference)
    }.sum / M //, targetSampleSize))).sum / M
    result
  }

  /**
    * Compute a partial pairwise deviation matrix for a given data set
    *
    * @param m The indexes from the original data ordered by the rank of the points
    * @return A 2-D Array contains the deviation for each pairwise dimension
    */
  def deviationMatrixPartial(m: I, dimensions: Array[Int]): Array[Array[Double]] = {
    // Sanity check
    m.refresh()
    //require(alpha > 0 & alpha < 1, "alpha should be greater than 0 and lower than 1")
    //require(M > 0, "M should be greater than 0")
    val numCols = m.ncols
    val matrix = Array.ofDim[Double](numCols, numCols) // We still have the full matrix but only fill some of the entries

    val cols = if (parallelize == 0) {
      dimensions.toSeq
    } else {
      val colspar = dimensions.par
      if (parallelize > 1) {
        //colspar.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallelize))
        colspar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      colspar
    }

    for {
      x <- cols //0 until numCols//.par // let's try not to parallelize here
      y <- (0 until numCols).filter(_ != x) // important otherwise problem set(0)
    } yield {
      //println(s"x: $x, y: $y, m.nrows: ${m.nrows}, m.ncols: ${m.ncols}")
      matrix(x)(y) = deviation(m, Set(x, y), x)
      //matrix(y)(x) = deviation(m, Set(x, y), y)
    }
    matrix
  }
}
