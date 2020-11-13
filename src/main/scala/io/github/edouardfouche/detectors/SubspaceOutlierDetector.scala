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
package io.github.edouardfouche.detectors

/**
  * Created by edouardfouche
  */
trait SubspaceOutlierDetector {
  val id: String

  /**
    * Compute the outlier score for each element of a given array of instances.
    *
    * @param instances instances (rows) of a data set.
    * @return An arrax of 2-Tuple. The first element is the index, the second is the LOF.
    */
  def computeScores(instances: Array[Array[Double]]): Array[(Int, Double)]
}
