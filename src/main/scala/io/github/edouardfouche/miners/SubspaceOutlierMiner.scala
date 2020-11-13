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

import breeze.stats.mean
import io.github.edouardfouche.detectors.FullSpaceOutlierDetector
import io.github.edouardfouche.preprocess.{DataRef, DataSet}
import io.github.edouardfouche.searchers.SubspaceSearcher

/**
  * Created by edouardfouche
  */
trait SubspaceOutlierMiner extends OutlierMiner {

  val subspaceSearcher: SubspaceSearcher
  val detector: FullSpaceOutlierDetector
  val max1000: Boolean
  val id: String

  /**
    * Compute outlier scores in a referenced data set
    *
    * @param ref reference to a data set (see document of DataRef)
    * @return An Array of 2-Tuple, the first element in the index in the data and the second the outlier score computed
    */
  def predict(ref: DataRef): Array[(Int, Double)] = {
    val subspaces = search(ref).map(_._2._1) // The special thing here is that the search is often done on a sample
    val data = ref.open().transpose // Let's make it column-oriented
    subspaces.map(x => {
      detector.computeScores(x.map(y => data(y)).toArray.transpose).map(_._2)
    }).transpose.map(mean(_)).zipWithIndex.map(x => (x._2, x._1))
  }

  /**
    * Return only the contrast subspaces (result of the subspace Searcher)
    *
    * @param ref reference to a Data Set
    * @return
    */
  def search(ref: DataRef): Array[(Int, (Set[Int], Double))] = {
    subspaceSearcher.search(ref, max1000)
  }

  def predict(data: Array[Array[Double]]): Array[(Int, Double)] = {
    val subspaces = search(data).map(_._2._1) // Be careful: Here the search will be done on the full data
    //val data = ref.open().transpose // Let's make it column-oriented
    subspaces.map(x => {
      detector.computeScores(x.map(y => data(y)).toArray.transpose).map(_._2)
    }).transpose.map(mean(_)).zipWithIndex.map(x => (x._2, x._1))
  }

  def search(data: Array[Array[Double]]): Array[(Int, (Set[Int], Double))] = {
    subspaceSearcher.search(data, max1000)
  }

  /**
    * Compute outlier scores in a referenced data set
    *
    * @param ref reference to a data set (see document of DataRef)
    * @return An Array of 2-Tuple, the first element in the index in the data and the second the outlier score computed
    */
  def predictWithSubspaces(ref: DataRef, subspaces: Array[Set[Int]]): Array[(Int, Double)] = {
    val data: DataSet = ref.open()
    predictWithSubspaces(data, subspaces)
  }

  /**
    * Compute outlier scores in a referenced data set
    *
    * @param data reference to a data set (row oriented)
    * @return An Array of 2-Tuple, the first element in the index in the data and the second the outlier score computed
    */
  def predictWithSubspaces(data: DataSet, subspaces: Array[Set[Int]]): Array[(Int, Double)] = {
    val thedata = data.columns // Let's make it column-oriented
    subspaces.map(x => {
      detector.computeScores(x.map(y => thedata(y)).toArray.transpose).map(_._2)
    }).transpose.map(mean(_)).zipWithIndex.map(x => (x._2, x._1))
  }

  def search(preprocessed: subspaceSearcher.test.I): Array[(Int, (Set[Int], Double))] = {
    subspaceSearcher.search(preprocessed)
  }

  def preprocess(ref: DataRef): subspaceSearcher.test.I = {
    val data: DataSet = ref.open(dropClass = true, max1000 = max1000)
    subspaceSearcher.preprocess(data)
  }
}
