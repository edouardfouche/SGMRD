/*
 * Copyright (C) 2020 Edouard Fouché
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

/**
  * Created by edouardfouche on 07.07.17.
  */
abstract class Stats {
  type D <: DimensionIndex
  type I <: Index[D]
  val id: String
  val alpha: Double
  val beta: Double
  val M: Int

  /**
    * @param input A data set (row oriented)
    */
  def preprocess(input: DataSet, stream: Boolean = false): I

  /**
    * @param m A data set (row oriented)
    */
  def contrast(m: DataSet, dimensions: Set[Int]): Double = {
    this.contrast(this.preprocess(m), dimensions)
  }

  def contrast(m: I, dimensions: Set[Int]): Double

  /**
    * @param m A data set (row oriented)
    */
  def contrastMatrix(m: DataSet): Array[Array[Double]] = {
    this.contrastMatrix(this.preprocess(m))
  }

  def contrastMatrix(m: I): Array[Array[Double]]
}
