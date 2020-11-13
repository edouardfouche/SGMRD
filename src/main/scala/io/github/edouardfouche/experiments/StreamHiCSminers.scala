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
import io.github.edouardfouche.preprocess._
import io.github.edouardfouche.streamminers.{HashMiner, LOFMiner, StreamMiner}
import io.github.edouardfouche.utils

/**
  * Created by edouardfouche
  */
object StreamHiCSminers extends Experiment {
  val nRep = 1

  val data: Vector[DataRef] = Vector(activity, kddcup99, example_10, example_20, example_50)
  val subspacepaths: Map[DataRef, Array[String]] = Map(activity -> Array(
    currentdir + "/StreamHiCS/results/activity.arff-StreamHiCS-1000-100-subspaces.txt"
  ), kddcup99 -> Array(
    currentdir + "/StreamHiCS/results/kddcup99.arff-StreamHiCS-1000-100-subspaces.txt"
  ), example_10 -> Array(
    currentdir + "/StreamHiCS/results/example_10_data.arff-StreamHiCS-1000-100-subspaces.txt"
  ), example_20 -> Array(
    currentdir + "/StreamHiCS/results/example_20_data.arff-StreamHiCS-1000-100-subspaces.txt"
  ), example_50 -> Array(
    currentdir + "/StreamHiCS/results/example_50_data.arff-StreamHiCS-1000-100-subspaces.txt"
  ))

  def run(): Unit = {
    info(s"Starting experiment - ${this.getClass.getSimpleName}")
    info(s"nrep: $nRep")
    info(s"Subspaces: ${subspacepaths mkString ","}")
    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val outlierminers: Array[StreamMiner] = Array(
      HashMiner(1000, 100, removeduplicates = true),
      LOFMiner(1, 1000, 100, removeduplicates = true),
      LOFMiner(2, 1000, 100, removeduplicates = true),
      LOFMiner(5, 1000, 100, removeduplicates = true),
      LOFMiner(10, 1000, 100, removeduplicates = true),
      LOFMiner(20, 1000, 100, removeduplicates = true),
      LOFMiner(50, 1000, 100, removeduplicates = true),
      LOFMiner(100, 1000, 100, removeduplicates = true),
    )

    val fullspaceminers: Array[StreamMiner] = Array(
      HashMiner(1000, 100, removeduplicates = true),
      LOFMiner(1, 1000, 100, removeduplicates = true),
      LOFMiner(2, 1000, 100, removeduplicates = true),
      LOFMiner(5, 1000, 100, removeduplicates = true),
      LOFMiner(10, 1000, 100, removeduplicates = true),
      LOFMiner(20, 1000, 100, removeduplicates = true),
      LOFMiner(50, 1000, 100, removeduplicates = true),
      LOFMiner(100, 1000, 100, removeduplicates = true),
    )

    for {
      rep <- 0 until nRep
    } {
      //info(s"This is repetition $rep")
      for {
        ref <- data
      } {
        info(s"Handling ${ref.id}")
        for {
          miner <- outlierminers.par
        } {
          for {
            subs <- subspacepaths(ref)
          } {
            info(s"${miner.id}: Handling ${subs}")
            val (results, time) = miner.predict(ref, subs)
            val labels: Array[Double] = ref.getLabels.map(x => if (x) 1.0 else 0.0)

            val rocauc = utils.getAreaUnderCurveOfROC(labels, results)
            val prauc = utils.getAreaUnderCurveOfPR(labels, results)
            val ap = utils.getAP(labels, results)
            val recallprecision = utils.getRecallPrecisionAt125etc(labels, results)

            val attributes = List("refId", "minerId", "subspaces", "minetime",
              "rocauc", "prauc", "ap", "p1", "p2", "p5", "p10", "p20", "p30",
              "r1", "r2", "r5", "r10", "r20", "r30")

            val summary = ExperimentSummary(attributes)

            summary.add("refId", ref.id)
            summary.add("minerId", miner.id)
            summary.add("subspaces", subs)
            summary.add("minetime", "%.4f".format(time))
            summary.add("rocauc", "%.4f".format(rocauc._1))
            summary.add("prauc", "%.4f".format(prauc._1))
            summary.add("ap", "%.4f".format(ap))

            summary.add("p1", "%.4f".format(recallprecision(0)._2))
            summary.add("p2", "%.4f".format(recallprecision(1)._2))
            summary.add("p5", "%.4f".format(recallprecision(2)._2))
            summary.add("p10", "%.4f".format(recallprecision(3)._2))
            summary.add("p20", "%.4f".format(recallprecision(4)._2))
            summary.add("p30", "%.4f".format(recallprecision(5)._2))

            summary.add("r1", "%.4f".format(recallprecision(0)._1))
            summary.add("r2", "%.4f".format(recallprecision(1)._1))
            summary.add("r5", "%.4f".format(recallprecision(2)._1))
            summary.add("r10", "%.4f".format(recallprecision(3)._1))
            summary.add("r20", "%.4f".format(recallprecision(4)._1))
            summary.add("r30", "%.4f".format(recallprecision(5)._1))

            summary.write(summaryPath)

          }
        }
        for {
          miner <- fullspaceminers.par
        } {
          info(s"Fullspace miner: ${miner.id}")

          val (results, time) = miner.predict(ref)
          val labels: Array[Double] = ref.getLabels.map(x => if (x) 1.0 else 0.0)

          val rocauc = utils.getAreaUnderCurveOfROC(labels, results)
          val prauc = utils.getAreaUnderCurveOfPR(labels, results)
          val ap = utils.getAP(labels, results)
          val recallprecision = utils.getRecallPrecisionAt125etc(labels, results)

          val attributes = List("refId", "minerId", "subspaces", "minetime",
            "rocauc", "prauc", "ap", "p1", "p2", "p5", "p10", "p20", "p30",
            "r1", "r2", "r5", "r10", "r20", "r30")

          val summary = ExperimentSummary(attributes)

          summary.add("refId", ref.id)
          summary.add("minerId", miner.id)
          summary.add("subspaces", "Full")
          summary.add("minetime", "%.4f".format(time))
          summary.add("rocauc", "%.4f".format(rocauc._1))
          summary.add("prauc", "%.4f".format(prauc._1))
          summary.add("ap", "%.4f".format(ap))

          summary.add("p1", "%.4f".format(recallprecision(0)._2))
          summary.add("p2", "%.4f".format(recallprecision(1)._2))
          summary.add("p5", "%.4f".format(recallprecision(2)._2))
          summary.add("p10", "%.4f".format(recallprecision(3)._2))
          summary.add("p20", "%.4f".format(recallprecision(4)._2))
          summary.add("p30", "%.4f".format(recallprecision(5)._2))

          summary.add("r1", "%.4f".format(recallprecision(0)._1))
          summary.add("r2", "%.4f".format(recallprecision(1)._1))
          summary.add("r5", "%.4f".format(recallprecision(2)._1))
          summary.add("r10", "%.4f".format(recallprecision(3)._1))
          summary.add("r20", "%.4f".format(recallprecision(4)._1))
          summary.add("r30", "%.4f".format(recallprecision(5)._1))

          summary.write(summaryPath)
        }
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${data.map(_.category).distinct mkString ","}")
  }
}
