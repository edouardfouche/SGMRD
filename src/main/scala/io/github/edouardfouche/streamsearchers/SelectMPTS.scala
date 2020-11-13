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

import io.github.edouardfouche.monitoring.bandits.external.MPTS
import io.github.edouardfouche.monitoring.scalingstrategies.NoScaling

case class SelectMPTS(nbsub: Int, threshold: Double) extends Selector {
  require(nbsub >= 1)
  val id = s"MPTS-$nbsub-$threshold"

  var bandit = MPTS(NoScaling(1), 1, 1, threshold)

  def select(subspaces: Array[(Int, (Set[Int], Double))]): Array[Int] = {
    if (bandit.narms == 1) {
      println("Initialize BANDIT")
      bandit = MPTS(NoScaling(nbsub.min(subspaces.length)),
        subspaces.length,
        nbsub.min(subspaces.length),
        threshold)
    }
    bandit.choosearms()
  }

  def observe(searched: Array[Int], result: Array[(Int, (Set[Int], Double))], current: Array[(Int, (Set[Int], Double))]) = {
    bandit.observe(searched, result, current)
  }
}
