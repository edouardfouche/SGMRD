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
import io.github.edouardfouche.monitoring.bandits.{BanditAdwin, BanditTS}
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * Multiple Play Thompson Sampling, combined with ADWIN
  * The idea of MP-TS comes from "Optimal Regret Analysis of Thompson Sampling in Stochastic Multi-armed BanditK Problem with Multiple Plays" (Komiyama 2016)
  * In "Scaling Multi-Armed Bandit Algorithms" (Fouché 2019), this is referred to as S-TS-ADWIN
  *
  * @param delta           the parameter for ADWIN (upper bound for the false positive rate)
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MPTS_ADWIN(delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS with BanditAdwin {
  val name = s"MP-TS-ADWIN-$delta"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val draws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1, x._1._2).draw())).sortBy(-_._2).take(k)
    val indexes = draws.map(_._1)
    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val updates = scala.collection.mutable.Map[Int, Double]()

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2
      beta_params(x._1) = (beta_params(x._1)._1 + d, beta_params(x._1)._2 + (1.0 - d))
      counts(x._1) += 1.0
      sums(x._1) += d

      // Add into adwin and add the update into the map
      sharedAdwin.addElement(x._1, d)
      updates(x._1) = d
      d
    })
    history = history :+ updates
    t += 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // Here we, add up the size of the adwin (those are the number of pulls) and the number of unpulls, to get the
    // actual size of window each arm.
    val windows = (0 until narms).map(x => (x, sharedAdwin.getSingleSize(x) + (history.length - counts(x))))

    val smallest_window = windows.minBy(_._2) // this is the smallest window

    // Rolling back
    if (smallest_window._2.toInt < history.length) {
      for {
        x <- smallest_window._2.toInt until history.length
      } {
        val rollback = history.head
        history = history.tail
        for ((key, value) <- rollback) {
          sums(key) = sums(key) - value // if(counts(key) == 1.0) 1.0 else weights(key) - (1.0/(counts(key)-1.0))*(value._1 - weights(key))
          counts(key) = counts(key) - 1 //- value._2
          beta_params(key) = (beta_params(key)._1 - value, beta_params(key)._2 - (1.0 - value))
        }
      }
    }
    t = history.length + 1 // The time context is the same as the history, which is the same as the smallest window

    (arms, gains, gains.sum)
  }

}
