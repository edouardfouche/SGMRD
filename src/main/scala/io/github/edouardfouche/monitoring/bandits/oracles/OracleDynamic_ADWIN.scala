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
package io.github.edouardfouche.monitoring.bandits.oracles

import io.github.edouardfouche.monitoring.bandits.BanditAdwin
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * A Dynamic Oracle with Multiple Plays, which always choose the top-k arms (not "in expectation") at each round.
  * It is combined with ADWIN (in fact, just for the scaling part)
  * This is like the "dynamic oracle" described in "Optimal Exploration-Exploitation in a Multi-Armed-Bandit Problem with Non-stationary Rewards" (Besbes14)
  *
  * @param delta           the parameter for ADWIN (upper bound for the false positive rate)
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class OracleDynamic_ADWIN(delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditAdwin {
  val name: String = s"OD-ADWIN-$delta"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    // OptimalBandit: compute the whole matrix and the choose the top-k arm with hinsight on this round
    val newMatrix = stream.nextAndCompute(combinations.zipWithIndex.map(_._2))
    if (newMatrix.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    // Find the top-k arms
    val diffMatrix = newMatrix.toArray.zip(currentMatrix.toArray).map(x => reward.getReward(x._1, x._2))

    // Update the current Matrix
    val topindexes = diffMatrix.zipWithIndex.sortBy(-_._1).map(_._2).take(k)
    val toparms = topindexes.map(combinations(_))

    val updates = scala.collection.mutable.Map[Int, Double]()

    topindexes.foreach(x => {
      val d = diffMatrix(x)
      currentMatrix(x) = newMatrix(x)
      counts(x) += 1.0
      sums(x) += d
      // Add into adwin and add the update into the map
      sharedAdwin.addElement(x, d)
      updates(x) = d
    })

    // Sum up the gain of the top arms / top indexes
    val gains = topindexes.map(diffMatrix(_))

    history = history :+ updates
    t += 1

    k = scalingstrategy.scale(gains, topindexes, sums, counts, t)

    // Here we, add up the size of the adwin (those are the number of pulls) and the number of unpulls, to get the
    // actual size of each pulled arm.
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
          sums(key) = sums(key) - value
          counts(key) = counts(key) - 1
        }
      }
    }
    t = history.length // The time context is the same as the history, which is the same as the smallest window

    (toparms, gains, gains.sum)
  }

}
