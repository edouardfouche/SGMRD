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
package io.github.edouardfouche.streamminers

import breeze.stats.mean
import io.github.edouardfouche.detectors.ElkiLOF
import io.github.edouardfouche.preprocess._

case class LOFMiner(k: Int, windowsize: Int, stepsize: Int, removeduplicates: Boolean) extends StreamMiner {
  val id = s"LOF-$k"

  // expectes row-oriented I guess
  def predict(data: DataSet, subspaces: SearchResult): Array[(Int, Double)] = {
    val subs: Array[Set[Int]] = if (!removeduplicates) subspaces.map(x => x._2._1)
    else subspaces.map(x => x._2._1).distinct

    val coldata = data.columns

    // could parallelize here
    val allscores: Array[Array[Double]] = subs.map(x =>
      ElkiLOF(k).computeScores(x.toArray.map(coldata(_)).transpose).map(_._2))

    allscores.transpose.map(x => mean(x)).zipWithIndex.map(x => (x._2, x._1))
  }


}
