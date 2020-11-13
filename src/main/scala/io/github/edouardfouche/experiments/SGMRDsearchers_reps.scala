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

package io.github.edouardfouche.experiments

import io.github.edouardfouche.experiments.Data._
import io.github.edouardfouche.mcde.KSP
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.streamsearchers._
import io.github.edouardfouche.utils
import io.github.edouardfouche.utils.SubspaceSearchTerminology

import scala.collection.parallel.immutable.ParVector

/**
  * Created by edouardfouche
  */
object SGMRDsearchers_reps extends Experiment with SubspaceSearchTerminology {
  val nRep = 3
  val data: Vector[DataRef] = Vector(pyro)

  val m = 100

  def run(): Unit = {
    info(s"Starting com.edouardfouche.experiments - ${this.getClass.getSimpleName}")
    info(s"nrep: $nRep")
    info(s"Datasets: ${data.map(_.id) mkString ","}")
    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val ks = KSP(m)

    val windowsize = 1000
    val stepsize = 1

    for {
      rep <- (1 to nRep).par
    } {
      info(s"This is repetition $rep")

      val outliersearchers: ParVector[DataRef => SGMRD] = ParVector(
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, stepsize, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, 2, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, 5, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, 10, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, 20, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, 50, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectRandom(1), windowsize, 100, parallelize = 0, monitoring = true),

        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, stepsize, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, 2, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, 5, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, 10, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, 20, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, 50, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectWorst(1), windowsize, 100, parallelize = 0, monitoring = true),

        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, stepsize, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, 2, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, 5, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, 10, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, 20, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, 50, parallelize = 0, monitoring = true),
        SGMRD(_, ks, 0.9, SelectMPTS(1, 0.0), windowsize, 100, parallelize = 0, monitoring = true),
      )

      for {
        ref <- data
      } {
        info(s"Handling ${ref.id}")

        for {
          searcherconstructor <- outliersearchers.par
        } {
          val searcher = searcherconstructor(ref)

          def export(subspaces: SearchResult, searched: Array[Int], success: Int,
                     indextime: Double, monitortime: Double, decisiontime: Double, searchtime: Double): Unit = {
            val attributes = List("refId", "refCategory", "searcherID", "nDim", "n", "searched", "ntried", "success",
              "indextime", "monitortime", "decisiontime", "searchtime", "nSubspaces", "avgSubspaceLength", "avgContrast", "i", "rep")
            val summary = ExperimentSummary(attributes)
            summary.add("refId", ref.id)
            summary.add("refCategory", ref.category)
            summary.add("searcherID", searcher.id)
            summary.add("nDim", searcher.ncols)
            summary.add("n", searcher.nrows)
            summary.add("searched", searched mkString "-")
            summary.add("ntried", searched.length)
            summary.add("success", success)
            summary.add("indextime", "%.4f".format(indextime))
            summary.add("monitortime", "%.4f".format(monitortime))
            summary.add("decisiontime", "%.4f".format(decisiontime))
            summary.add("searchtime", "%.4f".format(searchtime))
            summary.add("nSubspaces", subspaces.length)
            summary.add("avgSubspaceLength", subspaces.map(_._2._1.size).sum.toDouble / subspaces.length)
            summary.add("avgContrast", "%.4f".format(subspaces.map(_._2._2).sum / subspaces.length))
            summary.add("i", searcher.acc - 1)
            summary.add("rep", rep)
            summary.write(summaryPath)
            utils.saveSubspaces(subspaces.map(_._2), experiment_folder + s"/${ref.id}-${searcher.id}-subspaces-$rep.txt")
          }

          while (!searcher.isEmpty) {
            var (subspaces, searched, success, indextime, monitortime, decisiontime, searchtime) = searcher.getState
            export(subspaces, searched, success, indextime, monitortime, decisiontime, searchtime)
            searcher.next()
          }
          // Got to get the last state
          var (subspaces, searched, success, indextime, monitortime, decisiontime, searchtime) = searcher.getState
          export(subspaces, searched, success, indextime, monitortime, decisiontime, searchtime)
        }
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${data.map(_.category).distinct mkString ","}")
  }
}
