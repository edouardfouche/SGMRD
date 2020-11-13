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
package io.github.edouardfouche.streamminers

import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.utils
import io.github.edouardfouche.utils.{StopWatch, SubspaceSearchTerminology}

import scala.annotation.tailrec

trait StreamMiner extends SubspaceSearchTerminology {
  val id: String
  val windowsize: Int
  val stepsize: Int
  val removeduplicates: Boolean

  def evaluate(ref: DataRef, prediction: Array[(Int, Double)]): Double = {
    utils.getAreaUnderCurveOfROC(ref.getLabels.map(x => if (x) 1.0 else 0.0), prediction)._1
  }

  // TODO: Add F1, precision , recall , PR AUC (check out text outlier detection maybe)

  def predict(data: DataSet, subspaces: SearchResult): Array[(Int, Double)]

  def predict(ref: DataRef, subspacelistpath: String): (Array[(Int, Double)], Double) = {
    println(s"Reading subspacelist $subspacelistpath for ${ref.id} from file")
    val bufferedSource = scala.io.Source.fromFile(subspacelistpath)
    val lines = bufferedSource.getLines()
    val subspacelist: Array[Array[(Int, (Set[Int], Double))]] = lines.toArray.map(x =>
      x.split(";").zipWithIndex.map(x =>
        (x._2,
          (x._1.split(":")(0).split(",").map(_.replaceAll("\\s", "").toInt).toSet,
            x._1.split(":")(1).replaceAll("\\s", "").toDouble)))
    )
    bufferedSource.close()
    predict(ref, subspacelist)
  }

  def predict(ref: DataRef, subspacelist: Array[SearchResult]): (Array[(Int, Double)], Double) = {

    var offset = 0
    val fulldata: DataSet = ref.open(dropClass = true)

    println(s"fulldata.nrows: ${fulldata.nrows}")
    println(s"subspacelist.length: ${subspacelist.length}")
    require(fulldata.nrows - windowsize + 1 == subspacelist.length)

    val initdata: DataSet = fulldata.take(windowsize)
    val otherdata: DataSet = fulldata.drop(stepsize)

    val currentsubspaces: Array[(Int, (Set[Int], Double))] = subspacelist.head
    val othersubspaces = subspacelist.drop(stepsize)

    // initialize list of scores
    val scores: scala.collection.mutable.Map[Int, List[Double]] =
      scala.collection.mutable.Map[Int, List[Double]]((0 until fulldata.nrows).map(x => (x, List[Double]())): _*)

    // TODO: We could limit the prediction to the top-X subspaces
    val (firstscoringCPUtime, firstscoringWalltime, firstscores: Array[(Int, Double)]) =
      StopWatch.measureTime(predict(initdata, currentsubspaces))
    firstscores.foreach(x => scores(x._1 + offset) = x._2 :: scores(x._1 + offset))

    @tailrec
    def cumulative_scoring(restdata: DataSet, restsubspaces: Array[SearchResult], scoringtimes: List[Double]): Array[Double] = {
      if (restdata.nrows == 0 || restsubspaces.isEmpty) scoringtimes.reverse.toArray
      else {
        if (offset % 1000 == 0) println(s"Reached offset: $offset")
        val scoringdata = restdata.take(windowsize)
        val scoringsubspaces = restsubspaces.head
        val (scoringCPUtime, scoringWalltime, newscores: Array[(Int, Double)]) =
          StopWatch.measureTime(predict(scoringdata, scoringsubspaces))

        newscores.foreach(x => scores(x._1 + offset) = x._2 :: scores(x._1 + offset))
        offset += stepsize
        cumulative_scoring(restdata.drop(stepsize), restsubspaces.drop(stepsize), scoringCPUtime :: scoringtimes)
      }
    }

    offset = 100
    val scoringtime = cumulative_scoring(otherdata, othersubspaces, List(firstscoringCPUtime))

    val totalscores: Int = (0 until fulldata.nrows).map(x => scores(x).length).sum
    println(s"Did a total of $totalscores scores in ${scoringtime.sum} ms")

    ((0 until fulldata.nrows).map(x => (x, scores(x).sum / scores(x).length)).toArray, scoringtime.sum)
  }

  def predict(ref: DataRef): (Array[(Int, Double)], Double) = {

    var offset = 0
    val fulldata: DataSet = ref.open(dropClass = true)

    println(s"fulldata.nrows: ${fulldata.nrows}")

    val initdata: DataSet = fulldata.take(windowsize)
    val otherdata: DataSet = fulldata.drop(stepsize)

    val currentsubspaces: Array[(Int, (Set[Int], Double))] = Array((0, ((0 until fulldata.ncols).toSet, 0.0)))
    val othersubspaces = (0 until otherdata.nrows).toArray.map(x => Array((0, ((0 until fulldata.ncols).toSet, 0.0))))

    // initialize list of scores
    val scores: scala.collection.mutable.Map[Int, List[Double]] =
      scala.collection.mutable.Map[Int, List[Double]]((0 until fulldata.nrows).map(x => (x, List[Double]())): _*)

    val (firstscoringCPUtime, firstscoringWalltime, firstscores: Array[(Int, Double)]) =
      StopWatch.measureTime(predict(initdata, currentsubspaces))
    firstscores.foreach(x => scores(x._1 + offset) = x._2 :: scores(x._1 + offset))

    @tailrec
    def cumulative_scoring(restdata: DataSet, restsubspaces: Array[SearchResult], scoringtimes: List[Double]): Array[Double] = {
      if (restdata.nrows == 0 || restsubspaces.isEmpty) scoringtimes.reverse.toArray
      else {
        if (offset % 1000 == 0) println(s"Reached offset: $offset")
        val scoringdata = restdata.take(windowsize)
        val scoringsubspaces = restsubspaces.head
        val (scoringCPUtime, scoringWalltime, newscores: Array[(Int, Double)]) =
          StopWatch.measureTime(predict(scoringdata, scoringsubspaces))

        newscores.foreach(x => scores(x._1 + offset) = x._2 :: scores(x._1 + offset))
        offset += stepsize
        cumulative_scoring(restdata.drop(stepsize), restsubspaces.drop(stepsize), scoringCPUtime :: scoringtimes)
      }
    }

    offset = 100
    val scoringtime = cumulative_scoring(otherdata, othersubspaces, List(firstscoringCPUtime))

    val totalscores: Int = (0 until fulldata.nrows).map(x => scores(x).length).sum
    println(s"Did a total of $totalscores scores in ${scoringtime.sum} ms")

    ((0 until fulldata.nrows).map(x => (x, scores(x).sum / scores(x).length)).toArray, scoringtime.sum)
  }

}
