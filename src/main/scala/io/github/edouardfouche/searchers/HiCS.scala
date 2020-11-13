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
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * HiCS algorithm
  *
  * @param candidate_cutoff Maximal number of candidate during the candidate generation step
  * @param output_cutoff    Maximal number of subspaces in the output
  * @param test             Statistical test to be used (Kolmogorov-Smirnov or Mann-Whitney)
  */
case class HiCS(test: McdeStats, candidate_cutoff: Int = 400, output_cutoff: Int = 100,
                removeDuplicates: Boolean = true, parallelize: Int = 0) extends SubspaceSearcher() {
  val id = s"HiCS-${test.id}-$candidate_cutoff-$output_cutoff-" + s"$removeDuplicates"
  //require(alpha > 0 & alpha < 1, "alpha should be greater than 0 and lower than 1")
  //require(M > 0, "M should be greater than 0")
  require(candidate_cutoff > 0, "candidate_cutoff should be greater than 0")
  require(output_cutoff > 0, "output_cutoff should be greater than 0")

  /**
    * Perform the apriori-like heuristic to return a list of subspaces with high contrast. The list is ordered by
    * decreasing contrast and redundant subspaces are removed
    *
    * @param data An index matrix. It is a 2-D Array of 2-Tuple. The first element is the original index, the second the rank.
    * @return An Array of 2-D Tuple, where the first element is a subspace and the second element its contrast
    */
  override def search(data: test.I): SearchResult = {

    val subspaces2D = for {
      x <- data.indices
      y <- 0 until x
    } yield Set(x, y)

    //println(s"Number of 2-D candidates: ${subspaces2D.length}")

    // Question: How can we obtain all combinations as set ?
    //val initialSubspaces = subspaces2D.map(i => (i, test.contrast(data, i))).sortBy(-_._2).take(candidate_cutoff)
    // parallelized // probably not a good idea to parallelize at this level for the experiments
    //val initialSubspaces = subspaces2D.par.map( // parallelized
    //  i => (i, StatisticalTest.contrast(m, i, alpha, M))).seq.sortBy(-_._2).take(candidate_cutoff)

    //println("Reached that")
    val initialSubspaces = if (parallelize == 0) {
      subspaces2D.map(i => (i, test.contrast(data, i))
      ).sortBy(-_._2).take(candidate_cutoff).toArray
    } else {
      val subspaces2Dpar = subspaces2D.par
      if (parallelize > 1) {
        subspaces2Dpar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      subspaces2Dpar.map(i => (i, test.contrast(data, i))).toArray.sortBy(-_._2).take(candidate_cutoff)
    }
    //println("Reached this")


    // No way to parallelize that, since last step depends on the next one. At least do it tail recursive.
    @tailrec def generateSubspaces(candidates: Array[(Subspace, Score)], acc: Array[Array[(Subspace, Score)]]): Array[Array[(Subspace, Score)]] = {
      val newCandidates = for {
        x <- candidates.map(_._1)
        y <- candidates.map(_._1).filter(i => (x intersect i).size == (x.size - 1))
      } yield (x union y, (y -- x).toList.head)

      val candidateSubspaces = candidates.map(_._1)
      // maybe we can avoid the distinct somehow
      val prunedCandidates = newCandidates.filter(x => (x._1 - x._2).map(y => (x._1 - x._2) - y + x._2).forall(z => candidateSubspaces contains z)).map(_._1).distinct

      if (prunedCandidates.length == 0) acc
      else {
        //println(s"Number of ${prunedCandidates(0).size}-D candidates: ${prunedCandidates.length}")
        // parallelized, same remark about parallelization here
        //val newSubspaces = prunedCandidates.map(i => (i, test.contrast(data, i))).sortBy(-_._2)
        //val newSubspaces = prunedCandidates.par.map(i => (i, StatisticalTest.contrast(m, i, alpha, M))).seq.sortBy(-_._2)

        val newSubspaces = if (parallelize == 0) {
          prunedCandidates.map(i => (i, test.contrast(data, i))).sortBy(-_._2)
        } else {
          val prunedCandidatespar = prunedCandidates.par
          if (parallelize > 1) {
            prunedCandidatespar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          prunedCandidatespar.map(i => (i, test.contrast(data, i))).toArray.sortBy(-_._2)
        }

        // Prune here the last entry of acc (redundant entries)
        acc(acc.length - 1) = candidates.filter(x => !newSubspaces.exists(y => (x._1 subsetOf y._1) & (x._2 < y._2)))

        if (newSubspaces.head._2 < candidates.last._2) acc
        else {
          val prunedSubspaces = newSubspaces.take(candidate_cutoff)
          if (newSubspaces.head._1.size == data.ncols) acc :+ prunedSubspaces
          else generateSubspaces(prunedSubspaces, acc :+ prunedSubspaces)
        }
      }
    }

    val result = generateSubspaces(initialSubspaces, Array(initialSubspaces)).flatten
    val cutoff_result = result.sortBy(-_._2).take(output_cutoff).zipWithIndex.map(x => (x._2, x._1))

    //cutoff_result.foreach(println)
    cutoff_result
  }
}
