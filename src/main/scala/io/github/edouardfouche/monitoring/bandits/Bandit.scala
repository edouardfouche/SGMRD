/*
 * Copyright (C) 2018 Edouard Fouché
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
package io.github.edouardfouche.monitoring.bandits

import breeze.linalg
import com.typesafe.scalalogging.LazyLogging
import io.github.edouardfouche.monitoring.rewards.Reward
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import io.github.edouardfouche.streamsimulator.Simulator

/**
  * General trait of bandit algorithms
  */
trait Bandit extends LazyLogging {
  val name: String // Name of the bandit model
  val stream: Simulator // Stream (of data, or actions) that the bandit sees
  val reward: Reward // Criterion to determine whether the reward is 1 or 0, based on the outcome of the action
  val scalingstrategy: ScalingStrategy // strategy to let the number of pulls evolve from one time step to another
  val combinations: Array[(Int, Int)] = stream.pairs
  val ncols: Int = stream.ncols
  val narms: Int = stream.npairs
  val initial_k: Int = k

  require(k <= narms) // One cannot pull more arms than available
  var k: Int // number of pulls
  var initializationvalue = 1.0 // This is the value used for optimistic initilization // Set it to 0 for non-optimistic initialization
  var currentMatrix: linalg.Vector[Double] = init
  var sums: Array[Double] = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
  var counts: Array[Double] = sums.map(_ => initializationvalue)
  var t: Double = initializationvalue

  /**
    * Reset the bandit and the stream to their original state, just like at the object's creation.
    */
  def reset: Unit = {
    currentMatrix = init
    k = initial_k
    sums = (0 until narms).map(_ => initializationvalue).toArray
    counts = sums.map(_ => initializationvalue)
    t = initializationvalue
    stream.reset()
  }

  /**
    * In our study, every bandit holds a dependency matrix. Here, we initialize it.
    *
    * @return A matrix (in fact, squeezed to a 1-D vector) of zeros.
    */
  def init: linalg.Vector[Double] = linalg.Vector(combinations.map(x => 0.0))

  def disable_optimistic: Unit = { // Reset AND set the initializationvalue to 0.0 (not optimistic anymore)
    initializationvalue = 0.0
    currentMatrix = init
    k = initial_k
    sums = (0 until narms).map(_ => initializationvalue).toArray
    counts = sums.map(_ => initializationvalue)
    t = initializationvalue
    stream.reset()
  }

  /**
    * Obtain the next state of the stream, decide which arms to play and derive the corresponding reward
    *
    * @return An array of the pairs/arms choosing, the array of corresponding gains, and the sum of these gains
    */
  def next: (Array[(Int, Int)], Array[Double], Double)
}
