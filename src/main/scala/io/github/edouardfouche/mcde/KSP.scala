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

import io.github.edouardfouche.index.dimension.D_Rank
import io.github.edouardfouche.index.{I_Rank, I_Rank_Stream}
import io.github.edouardfouche.preprocess.DataSet

import scala.annotation.tailrec
import scala.math.{E, pow, sqrt}

/**
  * This is a re-implementation  of the contrast measure as proposed in HiCS, that is fitting the MCDE framework
  * Use the Kolmogorov-Smirnov test as basis. Approximate the p-value via marsaglia algorithm.
  * Does not use marginal restriction
  *
  * @param alpha Expected share of instances in slice (independent dimensions).
  * @param beta  Expected share of instances in marginal restriction (reference dimension).
  */
case class KSP(M: Int = 50, alpha: Double = 0.5, beta: Double = 0.5, var parallelize: Int = 0) extends McdeStats {
  type I = I_Rank
  type D = D_Rank
  val id = "KSP"

  override def getDIndexConstruct: Array[Double] => D = new D(_)

  override def getIndexConstruct: DataSet => I = new I(_)

  def preprocess(input: DataSet, stream: Boolean = false): I = {
    if (!stream) new I(input, parallelize)
    else new I_Rank_Stream(input, parallelize)
  }

  /**
    * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
    * a set of Int that correspond to the intersection of the position of the element in the slices in the other
    * dimensions.
    *
    * @param ref            The original position of the elements of a reference dimension ordered by their rank
    * @param indexSelection An array of Boolean where true means the value is part of the slice
    * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
    */
  // @param reference      The original position of the elements of a reference dimension ordered by their rank
  def twoSample(ref: D, indexSelection: Array[Boolean]): Double = {
    //require(ref.length == indexSelection.length, "reference and indexSelection should have the same size")
    val sliceStart = 0
    val sliceEnd = ref.length - 1

    //println(s"ref.currentvalues.length : ${ref.currentvalues.length}, indexSelection.length: ${indexSelection.length}")
    val theref = (sliceStart until sliceEnd).map(x => indexSelection(ref(x)._1))
    val inSlize = theref.count(_ == true)
    val outSlize = theref.length - inSlize

    if (inSlize == 0 || outSlize == 0) return 1.0 // If one is empty they are perfectly different --> score = 1 (and no prob with division by 0)

    val selectIncrement = 1.0 / inSlize
    val refIncrement = 1.0 / outSlize

    @tailrec def cumulative(n: Int, acc1: Double, acc2: Double, currentMax: Double): Double = {
      if (n == theref.length) currentMax
      else {
        //if (indexSelection(ref(n).position))
        if (theref(n))
          cumulative(n + 1, acc1 + selectIncrement, acc2, currentMax max math.abs(acc2 - (acc1 + selectIncrement)))
        else
          cumulative(n + 1, acc1, acc2 + refIncrement, currentMax max math.abs(acc2 + refIncrement - acc1))
      }
    }

    val D = cumulative(0, 0, 0, 0)
    val p = get_p_from_D(D, inSlize, outSlize)
    //println(s"ref.length: ${ref.length}, start: ${sliceStart}, end: ${sliceEnd}, inSlice: $inSlize, outSLice: $outSlize, inc1: ${selectIncrement}, inc2: $refIncrement, D: $D")
    p
  }

  /**
    * Convert the D value into a p-value
    *
    * @param D  D value from KSPmr test
    * @param n1 n Datapoints in first sample
    * @param n2 n Datapoints in second sample
    * @return p-value of two-sided two-sample KSPmr
    *
    *         This uses the approach from Marsaglia G, Tsang WW, Wang J (2003). "Evaluating Kolmogorov's Distribution". Journal of Statistical Software. 8 (18): 1–4. doi:10.18637/jss.v008.i18.
    *         See also:
    *         - https://stats.stackexchange.com/questions/389034/kolmogorov-smirnov-test-calculating-the-p-value-manually
    *         - https://stats.stackexchange.com/questions/149595/ks-test-how-is-the-p-value-calculated
    *         - https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Kolmogorov_distribution
    *         - https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#cite_note-2
    */
  def get_p_from_D(D: Double, n1: Long, n2: Long): Double = {
    lazy val z = D * sqrt(n1 * n2 / (n1 + n2))

    def exp(k: Int): Double = pow(-1, k - 1) * pow(E, -2 * pow(k, 2) * pow(z, 2))

    def infi_exp(k: Int): Double = pow(-1, k - 1) * pow(E, 2 * pow(k, 2) * pow(D, 2)) // in case lim n1, n2 -> infi

    // TODO: The part inside the summation could be done easily in parallel
    @tailrec
    def loop(summation: Double, i: Int, end: Int, f: Int => Double): Double = {
      if (i == end) f(i) + summation
      else loop(f(i) + summation, i + 1, end, f)
    }

    if (n1 >= 3037000499L && n2 >= 3037000499L) 1 - 2 * loop(0, 1, 1000, infi_exp) // squaring n1,n2 will reach the limit of Long
    else 1 - 2 * loop(0, 1, 1000, exp)
  }
}
