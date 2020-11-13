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

import io.github.edouardfouche.index.dimension.{D_CRank_Stream, D_Count_Stream, D_Rank_Stream, DimensionIndex}
import io.github.edouardfouche.preprocess.DataSet

import scala.collection.parallel.ForkJoinTaskSupport

// The index for a data set with heterogenous (mixed) types of columns, with stream operations
class I_Multi_Stream(data: DataSet, parallelize: Int = 0) extends I_Multi(data, parallelize) {
  override val id = "MultiStream"

  // basically, does nothing
  override def toStream: I_Multi_Stream = this

  /**
    * Initialize the index
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  override protected def createIndex(data: DataSet): Vector[DimensionIndex] = {
    if (parallelize == 0) {
      (0 until data.ncols).toVector.map(x => (data.types(x), data(x))).map {
        case ("n", x: Array[Double]) => new D_Rank_Stream(x)
        case ("o", x: Array[Double]) => new D_CRank_Stream(x)
        case ("c", x: Array[Double]) => new D_Count_Stream(x)
        case (_, _) => throw new Error(s"Unsupported type")
      }
    } else {
      val columns = (0 until data.ncols).par
      if (parallelize > 1) columns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      columns.toVector.map(x => (data.types(x), data(x))).map {
        case ("n", x: Array[Double]) => new D_Rank_Stream(x)
        case ("o", x: Array[Double]) => new D_CRank_Stream(x)
        case ("c", x: Array[Double]) => new D_Count_Stream(x)
        case (_, _) => throw new Error(s"Unsupported type")
      }
    }
  }

}
