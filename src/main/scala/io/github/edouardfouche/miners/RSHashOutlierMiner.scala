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
package io.github.edouardfouche.miners

import breeze.stats.distributions.Uniform
import io.github.edouardfouche.preprocess._

/**
  * Created by edouardfouche
  */
case class RSHashOutlierMiner(m: Int = 300) extends OutlierMiner {
  // m in the number of iterations
  val id = s"RSHash-$m"
  val p = 10000 // size of hash table
  val w = 4 // Number of hash functions

  // scores are the other way around for this approach
  //override def evaluate(ref: DataRef, prediction: Array[(Int, Double)]): Double = {
  //  utils.getAreaUnderCurveOfROC(ref.getLabels, prediction.map(- _._2))._1
  //} // Don't need to do that anymore since we do "- score" (to follow the idea that the higher the score, the more likely it is an outlier)

  def predict(ref: DataRef): Array[(Int, Double)] = {
    val D: DataSet = ref.open()
    predict(D.columns.transpose)
  }

  def predict(data: Array[Array[Double]]): Array[(Int, Double)] = {
    //val D = ref.open() // the data
    val d = data(0).length // number of dimensions
    val n = data.length // number of rows
    //println(s"d: $d, n: $n")
    val s = 1000.min(n) // s is the sample size, typically a small constant number of points, minimum between 1000 and the number of points

    val scores: Array[Array[Double]] = (1 to m).toArray.map { i =>
      val f = new Uniform(1.0 / math.sqrt(s), 1 - (1.0 / math.sqrt(s))).draw()
      val shiftvector: Array[Double] = (1 to d).toArray.map(x => new Uniform(0, f).draw())

      // Choose r dimensions at random (as a set d), (but maximum d dimensions)
      val r = new Uniform(1 + 0.5 * math.log(s) / math.log((2.0).max(1.0 / f)), math.log(s) / math.log((2.0).max(1.0 / f))).draw().round.toInt.min(d)
      val v = scala.util.Random.shuffle((0 until d).toList).take(r).toArray

      // Get a random sample S of size s from the data
      val indexes = if (s == n) data.indices.toArray else scala.util.Random.shuffle(data.indices.toList).take(s).toArray
      val S = if (s == n) data else indexes.map(x => data(x))
      // Get the min and max for each columns
      val minmax = S.transpose.map(y => (y.min, y.max))
      // Discard the selected dimensions for which min = max
      //val todelete = minmax.zipWithIndex.filter(x => x._1._1 == x._1._2).map(_._2)
      //val V = v.filter(x => !todelete.contains(x))
      val V = v.filter(x => minmax(x)._1 != minmax(x)._2)

      // Normalize the data
      val X = data.map(x => x.zipWithIndex.map(y => (y._1 - minmax(y._2)._1) / (minmax(y._2)._2 - minmax(y._2)._1)))
      val Xi = if (s == n) X else indexes.map(x => X(x))
      //val Xi = S.map(x => x.zipWithIndex.map(y => (y._1 - minmax(y._2)._1)/(minmax(y._2)._2 - minmax(y._2)._1)))

      // Discretize the data
      val Y = X.map(x => V.map(y => math.floor((x(y) + shiftvector(y)) / f)))
      val Yi = if (s == n) Y else indexes.map(x => Y(x))

      // define the random seeds for our hash functions
      val seeds = (1 to w).map(x => new Uniform(0, 100000).draw().toInt)
      // initialize hashtable
      var hashtable = (1 to w).toArray.map(x => (1 to p).toArray.map(y => 0))

      // Apply w different hash functions to Y -> h_1(Y), ..., h_w(Y), Increment the h_k(Y)th element of the kth hash table by 1
      for {
        element <- Yi
        seed <- seeds.zipWithIndex
      } {
        hashtable(seed._2)(math.abs(scala.util.hashing.MurmurHash3.arrayHash(element, seed._1) % p)) += 1
      }
      val minc = Y.map(element => seeds.zipWithIndex.map(seed => hashtable(seed._2)(math.abs(scala.util.hashing.MurmurHash3.arrayHash(element, seed._1) % p))).min)

      // Watch out the "-"
      val result: Array[Double] = if (s == n) {
        minc.zipWithIndex.map(x => -scala.math.log(x._1) / scala.math.log(2))
      } else {
        minc.zipWithIndex.map(x => {
          if (indexes.contains(x._2)) -scala.math.log(x._1) / scala.math.log(2)
          else -scala.math.log(x._1 + 1) / scala.math.log(2)
        })
      }
      result
    }

    scores.transpose.map(x => x.sum / m).zipWithIndex.map(x => (x._2, x._1))
  }
}
