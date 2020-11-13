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
import io.github.edouardfouche.mcde.KSP
import io.github.edouardfouche.searchers.{HiCS, SubspaceSearcher}

/**
  * Created by edouardfouche
  */
case class HiCSOutlierMiner(candidate_cutoff: Int = 400, output_cutoff: Int = 100, k: Int = 100,
                            max1000: Boolean = true) extends SubspaceOutlierMiner {
  val subspaceSearcher: SubspaceSearcher = HiCS(KSP(), candidate_cutoff, output_cutoff)
  val detector = ElkiLOF(k)
  val id = subspaceSearcher.id + "-" + detector.id + "-" + s"$max1000"(0)
}
