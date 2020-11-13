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
package io.github.edouardfouche.mcde

import io.github.edouardfouche.index.dimension.{D_CRank, D_Count, D_Rank, DimensionIndex}
import io.github.edouardfouche.index.{I_Multi, I_Multi_Stream}
import io.github.edouardfouche.preprocess.DataSet

/**
  * The MCDE Framework on heterogeneous data
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta  Expected share of instances in marginal restriction (reference dimension).
  */
case class MCDEP(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0) extends McdeStats {
  type D = DimensionIndex
  type I = I_Multi
  val id = "MCDEP"

  override def getDIndexConstruct: Array[Double] => DimensionIndex = new D_Rank(_) // because whatever...

  override def getIndexConstruct: DataSet => I_Multi = new I_Multi(_)

  def preprocess(input: DataSet, stream: Boolean = false): I = {
    if (!stream) new I(input, parallelize)
    else new I_Multi_Stream(input, parallelize)
  }

  /**
    * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
    * a set of Int that correspond to the intersection of the position of the element in the slices in the other
    * dimensions.
    *
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
  def twoSample(ref: DimensionIndex, indexSelection: Array[Boolean]): Double = {
    ref match {
      case x: D_Count =>
        CSP(M, alpha, beta, parallelize).twoSample(x, indexSelection)
      case x: D_Rank =>
        KSP(M, alpha, beta, parallelize).twoSample(x, indexSelection)
      case x: D_CRank =>
        MWP(M, alpha, beta, parallelize).twoSample(x, indexSelection)
      case _ => throw new Error("Unsupported DimensionIndex type")
    }
  }
}
