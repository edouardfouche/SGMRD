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

import io.github.edouardfouche.monitoring.bandits.Bandit
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * The Worst Oracle with Multiple Plays, which always choose the bottom-k arms at each round
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  * @note In fact, not useful at all. We use it simply to check what is the worst case scenario
  */
case class OracleWorst(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  val name: String = "OW"

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val newMatrix = stream.nextAndCompute(combinations.zipWithIndex.map(_._2))
    if (newMatrix.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    // Find the worst-k arms
    val diffMatrix = newMatrix.zip(currentMatrix.toArray).map(x => reward.getReward(x._1, x._2))

    val worstindexes = diffMatrix.zipWithIndex.sortBy(_._1).map(_._2).take(k)
    val worstarms = worstindexes.map(combinations(_))

    // Update the current Matrix
    worstindexes.foreach(x => {
      currentMatrix(x) = newMatrix(x)
      counts(x) += 1.0
      sums(x) += diffMatrix(x) // (http://www.cs.cmu.edu/~rsalakhu/10703/Lecture_Exploration.pdf slide 12)
    })

    val gains = worstindexes.map(diffMatrix(_))

    t += 1
    k = scalingstrategy.scale(gains, worstindexes, sums, counts, t)

    (worstarms, gains, gains.sum)
  }

}
