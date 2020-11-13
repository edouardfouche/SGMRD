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
package io.github.edouardfouche.utils

/**
  * Created by edouardfouche
  */
trait SubspaceSearchTerminology {
  /*
  abstract class Subspace extends Set[Int]
  abstract class Score extends Double
  abstract class Index extends Int
  abstract class Rank extends Float
  */
  type Subspace = Set[Int]
  type Score = Double
  //type Index = Int
  //type Dim = Int
  //type Rank = Float

  type SearchResult = Array[(Int, (Subspace, Score))]

  /*
  abstract class GMDResult extends SearchResult {
    override type A = Dim
    override type B = (Subspace, Score)
  }

  abstract class HiCSResult extends SearchResult {
    override type A = Subspace
    override type B = Score
  }
  */
  /*

  */
}
