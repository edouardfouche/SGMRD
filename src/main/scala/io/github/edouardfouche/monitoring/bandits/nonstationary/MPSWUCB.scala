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

import io.github.edouardfouche.monitoring.bandits.BanditUCB
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * Sliding-Window UCB with Multiple Plays
  * The idea of SW-UCB comes from "On Upper-Confidence Bound Policies for Non-Stationary Bandit Problems" (Garivier2011)
  *
  * @param windowsize      size of the sliding window
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  * @note Can be seen as an adaptation from "On Upper-Confidence Bound Policies for Switching Bandit Problems" (Garivier2011)
  */
case class MPSWUCB(windowsize: Int)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  require(windowsize > 1)

  val name = s"MP-SWUCB; w=$windowsize"

  var sumsbuffer: Array[Array[Double]] = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray
  var countsbuffer: Array[Array[Double]] = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray

  override def reset: Unit = {
    super.reset
    sumsbuffer = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray
    countsbuffer = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray
  }

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    //TODO: In that case I am actually not sure whether I should replace t by the sum of all the pulls
    val confidences = counts.map(x => if (t == 0.0 | x == 0.0) 0 else math.sqrt((logfactor * math.log(t.min(windowsize))) / x))

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2) //.min(1.0))

    val sortedupperconfidences = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)
    val indexes = sortedupperconfidences.take(k)
    val notindexes = sortedupperconfidences.drop(k)

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val bufferposition = ((t - 1) % windowsize).toInt
    // forget past values
    if (t - 1 >= windowsize) {
      counts.indices.foreach { x =>
        counts(x) -= countsbuffer(x)(bufferposition)
        sums(x) -= sumsbuffer(x)(bufferposition)
      }
    }

    notindexes.indices.foreach { x =>
      countsbuffer(x)(bufferposition) = 0
      sumsbuffer(x)(bufferposition) = 0.0
    }

    // Update the current Matrix, compute the gains and update the weights at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace

      counts(x._1) += 1.0
      sums(x._1) += d
      countsbuffer(x._1)(bufferposition) = 1
      sumsbuffer(x._1)(bufferposition) = d
      d
    })
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t.min(windowsize))

    val gain = gains.sum
    (arms, gains, gain)
  }

}
