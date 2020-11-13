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
package io.github.edouardfouche.monitoring.bandits.nonstationary

import io.github.edouardfouche.monitoring.bandits.Bandit
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * Discounted UCB with Multiple Plays
  * The idea of "Discounted" UCB comes from "On Upper-Confidence Bound Policies for Non-Stationary Bandit Problems" (Garivier2011)
  *
  * @param gamma           the discounting factor, applied to the beta parameters at each stage
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MPDUCB(gamma: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  require((0 <= gamma) & (gamma <= 1.0))

  val name = s"MP-DUCB; g=$gamma"

  val logfactor: Double = 3.0 / 2.0 // or 1.0 / 2.0 ?

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    t = t * gamma
    counts.map(x => x * gamma)
    sums.map(x => x * gamma)

    val confidences = counts.map(x => if (t == 0.0 | x == 0.0) 0 else math.sqrt((logfactor * math.log(t)) / x))

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2) //.min(1.0))

    val indexes = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2).take(k)

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace

      counts(x._1) += 1.0
      sums(x._1) += d
      d
    })
    t = t + 1.0

    k = scalingstrategy.scale(gains, indexes, sums, counts, t.toInt)

    val gain = gains.sum
    (arms, gains, gain)
  }

}
