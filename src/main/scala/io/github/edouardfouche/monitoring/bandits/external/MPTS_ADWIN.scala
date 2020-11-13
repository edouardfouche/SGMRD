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
import io.github.edouardfouche.monitoring.resetstrategies.SharedAdwin
import io.github.edouardfouche.monitoring.scalingstrategies.ScalingStrategy

/**
  * Multiple Play Thompson Sampling
  * The idea of MP-TS comes from "Optimal Regret Analysis of Thompson Sampling in Stochastic Multi-armed BanditK Problem with Multiple Plays" (Komiyama 2016)
  * In "Scaling Multi-Armed Bandit Algorithms" (Fouché 2019), this is referred to as S-TS
  *
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MPTS_ADWIN(delta: Double)(scalingstrategy: ScalingStrategy, val narms: Int, var k: Int, threshold: Double) {
  val name = "MP-TS-ADWIN-$delta"
  /**
    * In our study, every bandit holds a dependency matrix. Here, we initialize it.
    *
    * @return A matrix (in fact, squeezed to a 1-D vector) of zeros.
    */
  val initial_k: Int = k
  var beta_params: Array[(Double, Double)] = (0 until narms).map(x => (1.0, 1.0)).toArray // initialize beta parameters to 1
  var initializationvalue = 0.0 // This is the value used for optimistic initilization // Set it to 0 for non-optimistic initialization
  var sums: Array[Double] = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
  var counts: Array[Double] = sums.map(_ => initializationvalue)
  var t: Double = initializationvalue
  //val threshold = 0.1
  var sharedAdwin = new SharedAdwin(narms, delta)
  // add the optimistic initialization to the history and the adwin
  //val init_updates: scala.collection.mutable.Map[Int, Double] = scala.collection.mutable.Map[Int, Double]()
  //(0 until narms).foreach{x =>
  //  sharedAdwin.addElement(x, 1.0)
  //  init_updates(x) = 1.0
  //}
  //var history: List[scala.collection.mutable.Map[Int,Double]] = List(init_updates)
  var history: List[scala.collection.mutable.Map[Int, Double]] = List()

  def reset: Unit = {
    beta_params = (0 until narms).map(x => (1.0, 1.0)).toArray
    sharedAdwin = new SharedAdwin(narms, delta)
    history = List()
  }

  def choosearms(): Array[Int] = {
    //draws
    val draws: Array[(Int, Double)] = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1, x._1._2).draw())).sortBy(-_._2).take(k)
    val indexes = draws.map(_._1)
    indexes
  }

  def observe(searched: Array[Int], result: Array[(Int, (Set[Int], Double))], current: Array[(Int, (Set[Int], Double))]): Unit = {
    val rewards: Array[(Int, Double)] = searched.zipWithIndex.map(x =>
      if (threshold == 0) {
        if ((result(x._2)._2._1 != current(x._1)._2._1) && (result(x._2)._2._2 > current(x._1)._2._2)) (x._1, 1.0) else (x._1, 0.0)
        //val signal = (result(x._2)._2._2 - current(x._1)._2._2 + 1)/ 2
        //if(new Bernoulli(signal).draw()) (x._1,1.0) else (x._1,0.0)
      } else {
        if (current(x._1)._2._2 + threshold < result(x._2)._2._2) (x._1, 1.0) else (x._1, 0.0)
      }
    )
    println(s"STS-ADWIN: avgreward: ${rewards.map(_._2).sum / rewards.length}, searched: ${searched mkString ","}, rewards: ${rewards mkString ","}")
    println(s"STS-ADWIN: results: ${result.map(x => (x._1, x._2._1 mkString ";", "%.2f".format(x._2._2))) mkString ","}")
    println(s"STS-ADWIN: current: ${searched.map(x => (current(x)._1, current(x)._2._1 mkString ";", "%.2f".format(current(x)._2._2))) mkString ","}")
    println(s"STS-ADWIN: estimates: ${beta_params.zipWithIndex.map(x => s"${x._2}:${"%.2f".format(x._1._1 / (x._1._1 + x._1._2))} ") mkString "; "}")


    val updates = scala.collection.mutable.Map[Int, Double]()

    //Update
    rewards.foreach { x =>
      beta_params(x._1) = (beta_params(x._1)._1 + x._2, beta_params(x._1)._2 + (1.0 - x._2))
      counts(x._1) += 1.0
      sums(x._1) += x._2

      // Add into adwin and add the update into the map
      sharedAdwin.addElement(x._1, x._2)
      updates(x._1) = x._2
    }

    println(s"STS-ADWIN: sums: ${sums.sum}, counts: ${counts.sum}")

    // update the scaling
    history = history :+ updates
    t += 1
    k = scalingstrategy.scale(rewards.map(_._2), rewards.map(_._1), sums, counts, t)

    // Here we, add up the size of the adwin (those are the number of pulls) and the number of unpulls, to get the
    // actual size of window each arm.
    val windows = (0 until narms).map(x => (x, sharedAdwin.getSingleSize(x) + (history.length - (counts(x)))))

    val smallest_window = windows.minBy(_._2) // this is the smallest window

    println(s"smallest_window._2.toInt: ${smallest_window._2.toInt},  history.length: ${history.length}")
    // Rolling back
    if (smallest_window._2.toInt < history.length) {
      println(s"Rolling back! windows: ${(0 until narms).map(x => s"$x: ${sharedAdwin.getSingleSize(x)} + ${history.length} - ${counts(x)}") mkString " ; "}")
      println(s"history.length ${history.length}, params: ${beta_params.map { case (x, y) => s"$x: $y" } mkString ";"}")
      println(s"sums: ${sums mkString ";"}")
      println(s"counts: ${sums mkString ";"}")
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
      println(s"history.length ${history.length}, params: ${beta_params.map { case (x, y) => s"$x: $y" } mkString ";"}")
      println(s"sums: ${sums mkString ";"}")
      println(s"counts: ${sums mkString ";"}")
    }
    t = history.length //+ 1
  }
}
