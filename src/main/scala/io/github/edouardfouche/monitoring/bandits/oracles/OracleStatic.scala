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

import breeze.linalg
import io.github.edouardfouche.monitoring.bandits.Bandit
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * A Static Oracle with Multiple Plays, which always choose the top-k arms in expectation at each round
  * This is like the "static oracle" described in "Optimal Exploration-Exploitation in a Multi-Armed-Bandit Problem with Non-stationary Rewards" (Besbes14)
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class OracleStatic(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  val name: String = "OS"

  val cache: Array[Array[Double]] = stream.cache
  val top_indexes_hindsight: Array[Int] = get_top_indexes_hindsight
  val top_arms_hindsight: Array[(Int, Int)] = top_indexes_hindsight.map(combinations(_))

  def get_top_indexes_hindsight: Array[Int] = {
    val hindsightmatrix = currentMatrix
    var totalrewards: linalg.Vector[Double] = linalg.Vector(cache(0).indices.map(x => 0.0).toArray)
    for {
      x <- cache.indices
    } {
      val rewards = cache(x).zipWithIndex.map { x =>
        val r = reward.getReward(x._1, hindsightmatrix(x._2))
        hindsightmatrix(x._2) = r
        r
      }
      totalrewards = totalrewards + linalg.Vector(rewards)
    }
    totalrewards.toArray.zipWithIndex.sortBy(-_._1).map(_._2)
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val top_arms = top_arms_hindsight.take(k)
    val top_indexes = top_indexes_hindsight.take(k)

    val newValues = stream.nextAndCompute(top_indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val gains = (top_indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d // (http://www.cs.cmu.edu/~rsalakhu/10703/Lecture_Exploration.pdf slide 12)
      d
    })

    t += 1
    k = scalingstrategy.scale(gains, top_indexes, sums, counts, t)

    val gain = gains.sum
    (top_arms, gains, gain)
  }

}
