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
package io.github.edouardfouche.streamsearchers

case class SelectRandom(nbsub: Int) extends Selector {
  require(nbsub >= 1)
  val id = s"RD-$nbsub"

  def select(subspaces: Array[(Int, (Set[Int], Double))]): Array[Int] = scala.util.Random.shuffle(subspaces.indices.toList)
    .take(nbsub.min(subspaces.length)).toArray

  def observe(searched: Array[Int], current: Array[(Int, (Set[Int], Double))], previous: Array[(Int, (Set[Int], Double))]) = {
    //println(s"Selected random ${searched mkString ", "} )")
    //println(s"before: ${previous.map(x => s"${x._1}:[${x._2._1 mkString ","}]," + "%.2f".format(x._2._2)) mkString ","}")
    //println(s"now: ${current.map(x => s"${x._1}:[${x._2._1 mkString ","}]," + "%.2f".format(x._2._2)) mkString ","}")
    // Nothing
  }
}
