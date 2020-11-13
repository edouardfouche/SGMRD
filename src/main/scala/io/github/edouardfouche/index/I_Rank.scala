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

import io.github.edouardfouche.index.dimension.D_Rank
import io.github.edouardfouche.preprocess.DataSet

import scala.collection.parallel.ForkJoinTaskSupport

// The index for a numerical data set
class I_Rank(val data: DataSet, val parallelize: Int = 0) extends Index[D_Rank] {
  val id = "Rank"

  // Convert to the same index but with stream operations.
  def toStream: I_Rank_Stream = new I_Rank_Stream(data, parallelize)

  /**
    * Initialize the index
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  protected def createIndex(data: DataSet): Vector[D_Rank] = {
    if (parallelize == 0) {
      (0 until data.ncols).toVector.map(data(_)).map {
        case x: Array[Double] => new D_Rank(x)
        case x => throw new Error(s"Unsupported type of {${x mkString ","}}")
      }
    } else {
      val columns = (0 until data.ncols).par
      if (parallelize > 1) columns.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      columns.toVector.map(data(_)).map {
        case x: Array[Double] => new D_Rank(x)
        case x => throw new Error(s"Unsupported type of {${x mkString ","}}")
      }
    }
  }

}
