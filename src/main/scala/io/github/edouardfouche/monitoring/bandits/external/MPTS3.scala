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
package io.github.edouardfouche.monitoring.bandits.external

import breeze.stats.distributions.Beta
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy

/**
  * Multiple Play Thompson Sampling
  * The idea of MP-TS comes from "Optimal Regret Analysis of Thompson Sampling in Stochastic Multi-armed BanditK Problem with Multiple Plays" (Komiyama 2016)
  * In "Scaling Multi-Armed Bandit Algorithms" (Fouché 2019), this is referred to as S-TS
  *
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MPTS3(scalingstrategy: ScalingStrategy, val narms: Int, var k: Int) {
  val name = "MP-TS3"
  /**
    * In our study, every bandit holds a dependency matrix. Here, we initialize it.
    *
    * @return A matrix (in fact, squeezed to a 1-D vector) of zeros.
    */
  val initial_k: Int = k
  val threshold: Double = 0.2
  var beta_params: Array[(Double, Double)] = (0 until narms).map(x => (1.0, 1.0)).toArray // initialize beta parameters to 1
  var initializationvalue = 1.0 // This is the value used for optimistic initilization // Set it to 0 for non-optimistic initialization
  var sums: Array[Double] = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
  var counts: Array[Double] = sums.map(_ => initializationvalue)
  var t: Double = initializationvalue

  def reset: Unit = {
    beta_params = (0 until narms).map(x => (1.0, 1.0)).toArray
  }

  def choosearms(choosensubspaces: Array[(Int, (Set[Int], Double))], allsubspaces: Array[(Int, (Set[Int], Double))]): Array[Int] = {
    //draws
    val draws: Array[(Int, Double)] = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1, x._1._2).draw())).sortBy(-_._2).take(k)
    val indexes = draws.map(_._1)
    indexes
  }

  def observe(searched: Array[Int], result: Array[(Int, (Set[Int], Double))], current: Array[(Int, (Set[Int], Double))]): Unit = {
    val rewards: Array[(Int, Double)] = searched.zipWithIndex.map(x => if (current(x._1)._2._2 + threshold < result(x._2)._2._2) (x._1, 1.0) else (x._1, 0.0))
    //TODO: Maybe we don't need to apply such strong threshold, we could say +1/2 (then it is normalized between 0 and 1)

    println(s"searched: ${searched mkString ","}, rewards: ${rewards mkString ","}")
    println(s"results: ${result.map(x => (x._1, x._2._1 mkString ";", "%.2f".format(x._2._2))) mkString ","}")
    println(s"current: ${searched.map(x => (current(x)._1, current(x)._2._1 mkString ";", "%.2f".format(current(x)._2._2))) mkString ","}")

    rewards.foreach { x =>
      beta_params(x._1) = (beta_params(x._1)._1 + x._2, beta_params(x._1)._2 + (1.0 - x._2))
      counts(x._1) += 1.0
      sums(x._1) += x._2
    }

    // update the scaling
    t += 1
    k = scalingstrategy.scale(rewards.map(_._2), rewards.map(_._1), sums, counts, t)
  }
}
