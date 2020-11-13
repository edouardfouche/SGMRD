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

import breeze.stats.distributions.Beta
import io.github.edouardfouche.monitoring.bandits.BanditTS
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * Optimistic Thompson Sampling with Multiple Plays
  * The idea of "Optimistic" TS comes from "An Empirical Evaluation of Thompson Sampling" (Chapelle2011)
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MPOTS(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS {
  val name = "MP-OTS"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val draws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1, x._1._2).draw().max(x._1._1 / (x._1._1 + x._1._2)))).sortBy(-_._2).take(k)
    val indexes = draws.map(_._1)
    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      beta_params(x._1) = (beta_params(x._1)._1 + d, beta_params(x._1)._2 + (1.0 - d))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d
      d
    })

    t += 1
    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    (arms, gains, gains.sum)
  }

}
