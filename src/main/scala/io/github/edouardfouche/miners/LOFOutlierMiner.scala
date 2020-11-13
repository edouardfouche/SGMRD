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

import io.github.edouardfouche.detectors.ElkiLOF
import io.github.edouardfouche.preprocess._

/**
  * Created by edouardfouche
  */
case class LOFOutlierMiner(k: Int) extends OutlierMiner {
  val id = s"LOF-$k"

  def predict(ref: DataRef): Array[(Int, Double)] = {
    val data: DataSet = ref.open()
    predict(data.columns.transpose)
  }

  def predict(data: Array[Array[Double]]): Array[(Int, Double)] = {
    ElkiLOF(k).computeScores(data) // TODO: Is it maybe better with indexing?
  }
}
