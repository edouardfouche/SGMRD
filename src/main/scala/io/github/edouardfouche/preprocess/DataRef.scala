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
package io.github.edouardfouche.preprocess

import io.github.edouardfouche.index.Index
import io.github.edouardfouche.index.dimension.DimensionIndex
import io.github.edouardfouche.mcde.Stats

case class DataRef(id: String, path: String,
                   header: Int, separator: String, category: String, excludeIndex: Boolean = false) {

  /**
    * Open the data ref
    *
    * @param dropClass Whether to drop the "class" column if there is one. (assumes it is the last one)
    * @param max1000   cap the opened data to 1000 rows. If the original data has more rows, sample 1000 without replacement
    * @return A 2-D Array of Double containing the values from the csv. (row oriented)
    */
  def open(dropClass: Boolean = true, max1000: Boolean = false): DataSet = {
    try {
      Preprocess.open(path, header, separator, excludeIndex, dropClass, max1000)
    } catch {
      case e: Exception => println(s"Exception caught open $path" + e); null
    }
  }

  /**
    * Open the data ref and preprocess it according to a specific statistical test
    *
    * @param dropClass Whether to drop the "class" column if there is one. (assumes it is the last one)
    * @param max1000   cap the opened data to 1000 rows. If the original data has more rows, sample 1000 without replacement
    * @return A 2-D Array of Double containing the values from the csv, preprocessed. (column oriented)
    */
  def openAndPreprocess(test: Stats, dropClass: Boolean = true, max1000: Boolean = false): Index[_ <: DimensionIndex] = {
    test.preprocess(Preprocess.open(path, header, separator, excludeIndex, dropClass, max1000))
    // In case preprocess does wrong, this block becomes useful
    //try {
    //  test.preprocess(Preprocess.open(path, header, separator, excludeIndex, dropClass, max1000))
    //} catch {
    //  case e: Exception => println(s"Exception caught preprocessing $path" + e); null
    //}
  }

  /**
    * Return the labels of a data set
    *
    * @return An array of boolean (only boolean labels are supported currently)
    */
  def getLabels: Array[Boolean] = Preprocess.getLabels(path, header, separator, excludeIndex)

}