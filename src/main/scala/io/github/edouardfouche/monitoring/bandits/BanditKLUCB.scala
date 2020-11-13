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
package io.github.edouardfouche.monitoring.bandits

/**
  * General trait for bandits based on KL-UCB
  */
trait BanditKLUCB extends Bandit {
  val Ndelta: Double = scala.math.pow(10, -8)
  val eps: Double = scala.math.pow(10, -12)
  val maxiter = 20

  // use Newton's method
  def getKLUCBupper(arm: Int, t: Double): Double = {
    val logndn = scala.math.log(t) / counts(arm) // alternative: KL-UCB+ scala.math.log(t/ counts(arm)) / counts(arm)
    val p: Double = (sums(arm) / counts(arm)).max(Ndelta)
    if (p >= 1.0) return 1.0

    var q = p + Ndelta
    for (i <- 1 to maxiter) {
      val f = logndn - kl(p, q)
      val df = -dkl(p, q)
      if (f * f < eps) return q // newton's method has converged
      q = (1.0 - Ndelta).min((q - f / df).max(p + Ndelta))
    }
    q
  }

  // Note to future self: this was a big mistake, as operator ^ in scala does not mean at all "power
  //val delta = 1*10^(-8) // -> give -14
  //val eps = 1*10^(-12)
  // calculate the kl-divergence
  def kl(p: Double, q: Double): Double = {
    p * scala.math.log(p / q) + (1 - p) * scala.math.log((1 - p) / (1 - q))
  }

  // calculate the derivative kl-divergence
  def dkl(p: Double, q: Double): Double = {
    (q - p) / (q * (1.0 - q))
  }

}
