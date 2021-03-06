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

import breeze.stats.distributions.Beta
import io.github.edouardfouche.monitoring.bandits.BanditTS
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * Optimistic Discounted Thompson Sampling with Multiple Plays
  * The idea of "Optimistic" TS comes from "An Empirical Evaluation of Thompson Sampling" (Chapelle2011)
  * The idea of "Discounted" TS comes from "Taming Non-stationary Bandits: A Bayesian Approach" (Raj2017)
  *
  * @param gamma           the discounting factor, applied to the beta parameters at each stage
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MPDOTS(gamma: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS {
  require((0 <= gamma) & (gamma <= 1.0))
  val name = s"MP-DOTS; g=$gamma"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    t = t * gamma
    counts.map(x => x * gamma)
    sums.map(x => x * gamma)

    val sorteddraws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1, x._1._2).draw().max(x._1._1 / (x._1._1 + x._1._2)))).sortBy(-_._2)
    val indexes = sorteddraws.take(k).map(_._1)
    val notindexes = sorteddraws.drop(k).map(_._1)

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    // Update the current Matrix and compute the diff at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      beta_params(x._1) = ((beta_params(x._1)._1 * gamma + d).max(0.001), (beta_params(x._1)._2 * gamma + (1.0 - d)).max(0.001))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d
      d
    })

    t += 1.0
    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // We say the parameters cannot be lower than 0.001 because they need to be positive.
    // In some unfortunate circumstances, it might happen that rounding leads to a parameter becomes equal to 0
    // which create some error
    notindexes.foreach { x =>
      beta_params(x) = ((beta_params(x)._1 * gamma).max(0.001), (beta_params(x)._2 * gamma).max(0.001))
    }

    (arms, gains, gains.sum)
  }

}
