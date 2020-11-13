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
  * A Sequential Oracle with Multiple Plays, simply choosing K arms sequential at each round
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class OracleSequential(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  val name: String = "OSeq"

  var position = 0

  override def reset: Unit = {
    super.reset
    position = 0
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    // Draw k arms one after the other
    val indexes = (position until position + k).map(_ % narms).toArray
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

    position = (position + k) % narms

    t += 1
    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    val gain = gains.sum
    (arms, gains, gain)
  }
}
