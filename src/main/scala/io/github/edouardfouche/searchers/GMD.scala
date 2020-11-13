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
package io.github.edouardfouche.searchers

import io.github.edouardfouche.mcde.McdeStats

import scala.annotation.tailrec
import scala.collection.parallel._

/**
  * Created by edouardfouche
  *
  * @param test : Statistical test to be performed.
  */
case class GMD(test: McdeStats, removeDuplicates: Boolean = true, parallelize: Int = 0) extends SubspaceSearcher {
  val id = s"GMD-${test.id}-" + s"$removeDuplicates"

  /**
    * Perform the greedy heuristic to return a subspace per dimension with high deviation.
    * The list is ordered by dimensions
    *
    * @return An Array of 2-D Tuple, where the first element is a subspace and the second element its contrast.
    */
  def search(m: test.I): SearchResult = {
    val subspaces = m.indices.toArray

    //val (deviationMatrixTime, deviationMatrix) = StopWatch.measureWallTime(StatisticalTest.deviationMatrix(m, alpha, M))
    //println(s"Deviation Matrix Time:  ${deviationMatrixTime / 1000000000.0}s")
    val deviationMatrix = test.deviationMatrix(m)
    println("Deviation matrix precomputed !")

    def greedySearch(referenceDim: Int, candidate: (Subspace, Score)): (Subspace, Score) = {

      //println(s"${referenceDim}: ${deviationMatrix(referenceDim).zipWithIndex.filter(_._2 != referenceDim).sortBy(-_._1).map(x => s"(${x._1}, ${x._2})") mkString ","}")
      val candidatesDim = deviationMatrix(referenceDim).zipWithIndex.filter(_._2 != referenceDim).sortBy(-_._1).map(_._2)
      //println(s"CandidatesDim: ${candidatesDim mkString ","}")

      @tailrec def greedySearch_acc(candidate: (Subspace, Score), candidatesDim: Array[Int]): (Subspace, Score) = {
        if (candidatesDim.isEmpty) return candidate
        val newCandidate = candidate._1 + candidatesDim.head
        //println(s"Candidate: ${newCandidate mkString ","}")
        //require(newCandidate.size > 1, s"newCandidate size is equal to one. INFO: ${candidate._1}, ${candidatesDim.head}")
        val score = test.deviation(m, newCandidate, referenceDim)
        if (score > candidate._2) {
          //println(s"Newcandidate ! : ${newCandidate} with ${score}")
          greedySearch_acc((newCandidate, score), candidatesDim.tail)
        } else {
          greedySearch_acc(candidate, candidatesDim.tail)
        }
      }

      greedySearch_acc(candidate, candidatesDim)
    }

    //val (heuristicTime, result) = StopWatch.measureWallTime(subspaces.par.map(x => (x, greedySearch(x, (Set(x), 0.0)))).toArray.sortBy(_._1))
    //println("Heuristic Time: " + (heuristicTime / 1000000000.0) + "s")
    //result.groupBy(_._1).map(x => (x._1,x._2.map(_._2).max)).toArray // delete duplicates
    //result.foreach(println)
    //result.map(_._2)
    // TODO: Investigate the effects of parallelization (hint: they are good !)
    val searchresult: Array[(Int, (Subspace, Score))] = if (parallelize == 0) {
      subspaces.map(x => (x, greedySearch(x, (Set(x), 0.0))))
    } else {
      val subspacespar = subspaces.par
      if (parallelize > 1) {
        subspacespar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      subspacespar.map(x => (x, greedySearch(x, (Set(x), 0.0)))).toArray
    }

    //val result = if(!removeDuplicates) searchresult.sortBy(_._1)
    //else searchresult.map(_._2).groupBy(_._1).map(kv => (kv._1, kv._2.maxBy(_._2)._2)).toArray
    searchresult.sortBy(_._1)
  }
}
