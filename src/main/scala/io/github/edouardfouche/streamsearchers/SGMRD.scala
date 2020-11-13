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

import io.github.edouardfouche.mcde.McdeStats
import io.github.edouardfouche.preprocess.DataRef
import io.github.edouardfouche.utils.StopWatch

import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by edouardfouche
  */
case class SGMRD(ref: DataRef, test: McdeStats, gamma: Double, selector: Selector, windowsize: Int, stepsize: Int,
                 parallelize: Int = 0, stream: Boolean = true, monitoring: Boolean = true) extends StreamSearcher {
  val id = s"SGMRD-${test.id}-$gamma-${selector.id}-${windowsize}-${stepsize}" +
    s"-$parallelize" +
    s"-${if (stream) 1 else 0}" +
    s"-${if (monitoring) 1 else 0}"

  val formatter = new java.text.SimpleDateFormat("yyy-MM-dd-HH-mm")
  //var currentsubspaces: SearchResult = Array((0,(Set(0),0))) // This should be initialized with the first search

  val fulldata = ref.open(dropClass = true)
  val nrows = fulldata.nrows
  val ncols = fulldata.ncols

  val initdata = fulldata.take(windowsize)
  var (indextime, indexWalltime, currentindex) = StopWatch.measureTime(test.preprocess(initdata, stream = stream))
  var (searchtime, searchWalltime, currentsubspaces) = StopWatch.measureTime(search(currentindex, (0 until currentindex.ncols).toArray))
  var monitoredsubspaces = currentsubspaces
  var decisiontime = 0.0
  var monitortime = 0.0

  var searched: Array[Int] = (0 until fulldata.ncols).toArray
  var currentdata = fulldata.drop(windowsize)
  var acc = 1
  var success = 0

  println(s"Initialized searcher with ${currentdata.nrows} rows and ${currentdata.ncols} cols ($windowsize, $stepsize)")

  // of interest:  currentsubspaces, indextime, searchtime, decisiontime // internal: currentindex, currentdata,
  def getState: (SearchResult, Array[Int], Int, Double, Double, Double, Double) = (monitoredsubspaces, searched, success, indextime, monitortime, decisiontime, searchtime)

  def isEmpty: Boolean = if (currentdata.nrows == 0) true else false

  def next(): Unit = {
    if (currentdata.nrows == 0) {
      throw new Error("SGMRD iterator is empty")
    } else {
      if (acc % 1000 == 0) println(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - $id, : acc: $acc,  remaining ${currentdata.nrows}")
      if (acc % stepsize == 0) {
        val (insertCPUtime, insertWalltime, nothing) = StopWatch.measureTime({
          currentindex.insert(currentdata.head)
          currentindex.refresh()
        })
        indextime = insertCPUtime

        // We keep a separate knowledge of contrast
        val (monitorCPUtime, monitorWalltime, updatedsubspaces) = StopWatch.measureTime(monitoredsubspaces.map(x => (x._1, (x._2._1, x._2._2 * gamma + (1 - gamma) * test.deviation(currentindex, x._2._1, x._1)))))
        //val (monitorCPUtime, monitorWalltime, updatedsubspaces) = StopWatch.measureTime(currentsubspaces.map(x => (x._1, (x._2._1, test.deviation(currentindex, x._2._1, x._1)))))
        monitortime = monitorCPUtime
        //currentsubspaces = updatedsubspaces
        monitoredsubspaces = updatedsubspaces

        // Check if that is OK
        //val (monitorCPUtime2, monitorWalltime2, updatedsubspaces2) = StopWatch.measureTime(currentsubspaces.map(x => (x._1, (x._2._1, x._2._2*gamma + (1-gamma)*test.deviation(currentindex, x._2._1, x._1)))))
        //currentsubspaces = updatedsubspaces2

        val (decisionCPUtime, decisionWalltime, tosearch: Array[Int]) = StopWatch.measureTime(selector.select(currentsubspaces))
        searched = tosearch
        decisiontime = decisionCPUtime

        println(s"$id, (reached $acc) trigger the search for ${tosearch.length} subspaces: ${tosearch mkString ","}")

        success = 0
        val (searchCPUtime, searchWalltime, newsubspaces) = if (!tosearch.isEmpty) {

          StopWatch.measureTime {
            val result = search(currentindex, tosearch) // WARNING: May only return a few subspaces (e.g., for random selector), need to replace them  into current
            selector.observe(tosearch, result, monitoredsubspaces)

            tosearch.zip(result).foreach { x =>
              // record a success if the search led to improved contrast and a different subspace
              if ((monitoredsubspaces(x._1)._2._2 < x._2._2._2) && (monitoredsubspaces(x._1)._2._1 != x._2._2._1)) {
                currentsubspaces(x._1) = x._2
                monitoredsubspaces(x._1) = x._2
                success += 1
              } else {
                currentsubspaces(x._1) = x._2
                monitoredsubspaces(x._1) = x._2
              }
            }
            currentsubspaces
          }
        } else { // then just monitor the contrast
          // // monitor if not above
          //val (monitorCPUtime, monitorWalltime, updatedsubspaces) = StopWatch.measureTime(currentsubspaces.map(x => (x._1, (x._2._1, x._2._2*gamma + (1-gamma)*test.deviation(currentindex, x._2._1, x._1)))))
          //monitortime = monitorCPUtime
          //currentsubspaces = updatedsubspaces
          (0.0, 0.0, currentsubspaces)
        }
        searchtime = searchCPUtime
        currentsubspaces = newsubspaces

        currentdata = currentdata.tail
        acc += 1
      } else {
        searched = Array()
        success = 0

        val (insertCPUtime, insertWalltime, nothing) = StopWatch.measureTime(currentindex.insert(currentdata.head))
        indextime = insertCPUtime

        if (monitoring) {
          val (refreshCPUtime, refreshWalltime, nothing) = StopWatch.measureTime(currentindex.refresh())
          val (monitorCPUtime, monitorWalltime, updatedsubspaces: Array[(Int, (Subspace, Score))]) = StopWatch.measureTime(
            monitoredsubspaces.map(x => (x._1, (x._2._1, x._2._2 * gamma + (1 - gamma) * test.deviation(currentindex, x._2._1, x._1)))))

          //Check if that is OK
          //val (monitorCPUtime2, monitorWalltime2, updatedsubspaces2) = StopWatch.measureTime(currentsubspaces.map(x => (x._1, (x._2._1, x._2._2*gamma + (1-gamma)*test.deviation(currentindex, x._2._1, x._1)))))
          //currentsubspaces = updatedsubspaces2

          //currentsubspaces.map(x => (x._1, (x._2._1, test.deviation(currentindex, x._2._1, x._1)))))
          indextime = indextime + refreshCPUtime
          monitortime = monitorCPUtime
          monitoredsubspaces = updatedsubspaces
        } else { // This is just to save time basically
          //currentsubspaces = currentsubspaces
          monitortime = 0.0
        }
        decisiontime = 0.0
        searchtime = 0.0

        currentdata = currentdata.tail
        acc += 1
      }
    }
  }

  /**
    * Perform the greedy heuristic to return a subspace per dimension with high deviation.
    * The list is ordered by dimensions
    *
    * @return An Array of 2-D Tuple, where the first element is a subspace and the second element its contrast.
    */

  def search(m: test.I, dimensions: Array[Int]): SearchResult = {
    val subspaces = dimensions

    //val (deviationMatrixTime, deviationMatrix) = StopWatch.measureWallTime(StatisticalTest.deviationMatrix(m, alpha, M))
    //println(s"Deviation Matrix Time:  ${deviationMatrixTime / 1000000000.0}s")
    //println(s"About to do the deviation maatrix. m.ncols: ${m.ncols}, m.nrows: ${m.nrows}, dimensions: ${dimensions mkString ","}")
    val deviationMatrix = test.deviationMatrixPartial(m, subspaces)
    //println("Deviation matrix precomputed !")

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
        //if ((candidate._1.size <= 2) ||
        //  ((score > candidate._2) && (math.exp(-2 * test.M * math.pow((score - candidate._2),2))<0.01))
        if (score > candidate._2
        ) { // (math.exp(-2*test.M*math.pow((score - candidate._2),2))<0.01)
          //println(s"Newcandidate ! : ${newCandidate} with ${score}")
          greedySearch_acc((newCandidate, score), candidatesDim.tail)
        } else {
          greedySearch_acc(candidate, candidatesDim.tail)
        }
      }

      greedySearch_acc(candidate, candidatesDim)
    }

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
    searchresult //.sortBy(_._1) // yeah do not sort at this point !
  }

}
