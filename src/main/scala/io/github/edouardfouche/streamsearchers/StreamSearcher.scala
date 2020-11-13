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
package io.github.edouardfouche.streamsearchers

import io.github.edouardfouche.mcde.McdeStats
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.utils.SubspaceSearchTerminology

/**
  * Created by edouardfouche
  */
trait StreamSearcher extends SubspaceSearchTerminology {
  val test: McdeStats
  val selector: Selector
  val windowsize: Int
  val stepsize: Int

  val parallelize: Int
  val stream: Boolean
  val monitoring: Boolean

  val id: String

  def search(m: test.I, dimensions: Array[Int]): SearchResult

  //def search(ref: DataRef, max1000: Boolean): SearchResult = {
  //  val data = ref.open(dropClass=true, max1000=max1000)
  //  val preprocessed = test.preprocess(data)
  //  search(preprocessed)
  //}

  def preprocess(data: DataSet): test.I = {
    test.preprocess(data)
  }
}
