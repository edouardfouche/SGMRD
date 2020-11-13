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
package io.github.edouardfouche.monitoring.bandits.stationary

import io.github.edouardfouche.monitoring.bandits.BanditUCB
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * CUCB as described in "Combinatorial Multi-Armed BanditK: General Framework, Results and Applications" (Chen2013)
  * This version is combined with ADWIN
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  * @note when logfactor = 3/2, this is indeed as described in Chen 2013
  * @note when logfactor = 1/2, this is as "Thompson Sampling for Combinatorial Semi-Bandits" (Wang 2018) (Named CUCB-m therein)
  * @note the implementation is actually closer to "Improving Regret Bounds for Combinatorial Semi-Bandits with Probabilistically Triggered Arms and Its Applications" (Wang 2017)
  */
case class CUCBm(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  override val logfactor = 1.0 / 2.0 // this is the only difference
  val name = "CUCBm"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val confidences = counts.map(x => if (t == 0 | x == 0.0) 0 else math.sqrt((logfactor * math.log(t)) / x))

    val upperconfidences: Array[Double] = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2) //.min(1.0))

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
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t) // Scale it

    val gain = gains.sum
    (arms, gains, gain)
  }
}
