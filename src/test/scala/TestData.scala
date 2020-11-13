/*
 * Copyright (C) 2018 Edouard Fouché
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

import io.github.edouardfouche.mcde._
import io.github.edouardfouche.preprocess.{DataSet, Preprocess}

/**
  * Created by edouardfouche on 12.07.17.
  */
trait TestData {
  // note: ranking according to mann-whitney should not have any consequence of kolmogorov results
  val highcontrast_2D: DataSet = Preprocess.openCSV(
    getClass.getResource("/data/Linear-2-0.0.csv").getPath)
  val lowcontrast_2D: DataSet = Preprocess.openCSV(
    getClass.getResource("/data/Independent-2-0.0.csv").getPath)

  val highcontrast_5D: DataSet = Preprocess.openCSV(
    getClass.getResource("/data/Linear-5-0.0.csv").getPath)
  val lowcontrast_5D: DataSet = Preprocess.openCSV(
    getClass.getResource("/data/Independent-5-0.0.csv").getPath)

  val lowcontrast_100D: DataSet = Preprocess.openCSV(
    getClass.getResource("/data/Independent-100-0.0.csv").getPath)

  val mwp = MWP(100, 0.5)
  val kspn = KSP(100, 0.5)

  val input = Vector(mwp, kspn) // csp, too long because the data is continuous
}
