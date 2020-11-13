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
package io.github.edouardfouche.miners

import io.github.edouardfouche.preprocess.DataRef
import io.github.edouardfouche.utils

/**
  * Created by edouardfouche
  */
trait OutlierMiner {
  val id: String

  /**
    * Compute outlier scores in a referenced data set
    *
    * @param ref reference to a data set (see document of DataRef)
    * @return An Array of 2-Tuple, the first element in the index in the data and the second the outlier score computed
    */
  def predict(ref: DataRef): Array[(Int, Double)]

  def predict(data: Array[Array[Double]]): Array[(Int, Double)]

  def predictAndEvaluate(ref: DataRef): Double = {
    val prediction = predict(ref)
    evaluate(ref, prediction)
  }

  def evaluate(ref: DataRef, prediction: Array[(Int, Double)]): Double = {
    utils.getAreaUnderCurveOfROC(ref.getLabels.map(x => if (x) 1.0 else 0.0), prediction)._1
  }
}
