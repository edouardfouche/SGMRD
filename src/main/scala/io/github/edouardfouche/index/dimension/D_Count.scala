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
package io.github.edouardfouche.index.dimension

import scala.collection.mutable

/**
  * The index for a categorical dimension (intended for CSPmr)
  *
  * @param initvalues An array of values corresponding to the values in a column
  */
class D_Count(val initvalues: Array[Double]) extends DimensionIndex {
  type T = (Vector[Int], Int)
  val id = "Count"
  var currentvalues = initvalues.toVector

  var dindex: mutable.Map[Double, T] = createDimensionIndex(initvalues.toVector)

  def apply(i: Double): (Vector[Int], Int) = dindex(i)

  def refresh(): Unit = {}

  def insert(newpoint: Double): Unit = { // Recompute the dimensionindex from scratch on the new window, DimensionIndexStream must override
    currentvalues = currentvalues.drop(1) :+ newpoint
    dindex = createDimensionIndex(currentvalues)
  }

  def createDimensionIndex(input: Vector[Double]): mutable.Map[Double, T] = {
    val map = mutable.Map[Double, (Vector[Int], Int)]()
    for {
      x <- input.indices
    } {
      val current: (Vector[Int], Int) = map.getOrElse(input(x),
        (Vector[Int](), 0))
      val c: Vector[Int] = current._1
      map(input(x)) = (c :+ x, current._2 + 1) // we use it like a queue so that we can know efficiently which to delete.
    }
    map.map({ case (x, y) => (x, (y._1, y._2)) })
  }

  def uniformslice(sliceSize: Int): Array[Boolean] = slice(sliceSize)

  override def slice(sliceSize: Int): Array[Boolean] = {
    val logicalArray = Array.fill[Boolean](length)(false)
    val selectedCategories: Array[Double] = selectSlice(sliceSize)
    val selectedIndexes: Array[Int] = selectedCategories.flatMap(x => dindex(x)._1)

    // Correcting the slice size
    val c_selectedIndexes: Array[Int] = if (selectedIndexes.length > sliceSize) {
      scala.util.Random.shuffle(selectedIndexes.toList).drop(selectedIndexes.length - sliceSize).toArray
    } else if (selectedIndexes.length < sliceSize) {
      val othercategories = dindex.keys.filter(!selectedCategories.contains(_))
      val otherindexes = othercategories.flatMap(x => dindex(x)._1)
      selectedIndexes ++ scala.util.Random.shuffle(otherindexes.toList).take(sliceSize - selectedIndexes.length)
    } else selectedIndexes

    c_selectedIndexes.foreach(x => logicalArray(x) = true)
    logicalArray
  }

  def selectSlice(sliceSize: Int): Array[Double] = {
    val ratio = sliceSize.toDouble / initvalues.length.toDouble
    val categories = dindex.keys
    val toselect: Int = math.floor(categories.size * ratio).toInt.max(1).min(categories.size - 1) // Make sure at least 1, a most ncategories - 1
    scala.util.Random.shuffle(categories.toList).take(toselect).toArray
  }

  def selectRestriction(sliceSize: Int): Array[Double] = {
    val ratio = sliceSize.toDouble / initvalues.length.toDouble
    val categories = dindex.keys
    val toselect: Int = math.floor(categories.size * ratio).toInt.max(2).min(categories.size) // Make sure at least 2, a most ncategories
    scala.util.Random.shuffle(dindex.keys.toList).take(toselect).toArray
  }
}
