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

import io.github.edouardfouche.detectors.FullSpaceOutlierDetector
import io.github.edouardfouche.searchers.SubspaceSearcher

/**
  * Created by edouardfouche
  */
case class CustomSubspaceOutlierMiner(subspaceSearcher: SubspaceSearcher, detector: FullSpaceOutlierDetector,
                                      max1000: Boolean = true) extends SubspaceOutlierMiner {
  val id = subspaceSearcher.id + "-" + detector.id + "-" + s"$max1000"(0)
}
