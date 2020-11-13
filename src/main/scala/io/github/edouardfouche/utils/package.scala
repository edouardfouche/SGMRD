package io.github.edouardfouche

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

import java.io.{BufferedWriter, File, FileWriter}

import scala.annotation.tailrec

/**
  * Created by edouardfouche
  */
package object utils {
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("Time: " + (System.nanoTime - s) / 1e6 + "ms")
    ret
  }

  // expect rows
  def saveDataSet[T](res: Array[Array[T]], path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s"${(1 to res(0).length) mkString ","} \n") // a little header
    res.foreach(x => bw.write(s"${x mkString ","} \n"))
    bw.close()
  }


  def save[T](res: Array[T], path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(res mkString ",")
    bw.close()
  }

  def saveSubspaces(res: Array[(Set[Int], Double)], path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file, true))
    val str = res.map(x => s"${x._1 mkString ","} : " + "%.4f".format(x._2)) mkString " ; "
    bw.append(s"$str \n")
    //res.foreach(x => bw.append(s"${x._1 mkString ","} : ${x._2} \n"))
    bw.close()
  }

  def createFolderIfNotExisting(path: String): Unit = {
    val directory = new File(path)
    if (!directory.exists()) {
      directory.mkdir()
    }
  }

  def initiateSummaryCSV(path: String, header: String): FileWriter = {
    val fileA = new File(path)
    val fwA = new FileWriter(fileA, true) // append set to true
    fwA.write(header) // this is the header
    fwA
  }

  def extractFieldNames[T <: Product](implicit m: Manifest[T]) = m.runtimeClass.getDeclaredFields.map(_.getName)

  // matches with sklearn.metrics.roc_curve (auc)
  def getAreaUnderCurveOfROC(labels: Array[Double], predictions: Array[(Int, Double)]): (Double, Array[(Double, Double)]) = {
    require(labels.length == predictions.length, "labels and predictions should have the same length")
    val fpr_tpr = getROC(labels.map(x => if (x == 1.0) true else false), predictions.map(_._2))

    @tailrec def cumulative(fpr_tpr: Array[(Double, Double)], res: Double): Double = {
      if (fpr_tpr.length == 1) res
      else cumulative(fpr_tpr.tail, res + (fpr_tpr.tail.head._1 - fpr_tpr.head._1) * fpr_tpr.head._2)
    }

    (cumulative(fpr_tpr, 0.0), fpr_tpr)
  }

  // Should return (False positive rate, True positive rate)
  // https://en.wikipedia.org/wiki/Receiver_operating_characteristic
  // matches with sklearn.metrics.roc_curve
  def getROC(labels: Array[Boolean], predictions: Array[Double]): Array[(Double, Double)] = {
    require(labels.length == predictions.length, "labels and predictions should have the same length")
    val sortedPairs = labels.zip(predictions).sortBy(-_._2) // order the pairs by decreasing order of prediction
    val nP = labels.count(_ == true).toDouble // number of positives
    val nN = labels.length - nP // number of negatives
    val fP_inc = 1 / nN
    val tP_inc = 1 / nP

    @tailrec def cumulative(sortedPairs: Array[(Boolean, Double)], acc: (Double, Double), result: Array[(Double, Double)]): Array[(Double, Double)] = {
      if (sortedPairs.isEmpty) result
      else if (sortedPairs.head._1) {
        val newAcc = (acc._1, acc._2 + tP_inc)
        cumulative(sortedPairs.tail, newAcc, result :+ newAcc)
      } else {
        val newAcc = (acc._1 + fP_inc, acc._2)
        cumulative(sortedPairs.tail, newAcc, result :+ newAcc)
      }
    }

    cumulative(sortedPairs, (0, 0), Array())
  }

  // Turns out to return the same as AP now.
  // however, not inline with pr auc from scikit-learn
  def getAreaUnderCurveOfPR(labels: Array[Double], predictions: Array[(Int, Double)]): (Double, Array[(Double, Double)]) = {
    require(labels.length == predictions.length, "labels and predictions should have the same length")
    val fpr_tpr = getPR(labels.map(x => if (x == 1.0) true else false), predictions.map(_._2)).sortBy(_._2)

    @tailrec def cumulative(fpr_tpr: Array[(Double, Double)], res: Double): Double = {
      if (fpr_tpr.length == 1) res
      else cumulative(fpr_tpr.tail, res + (fpr_tpr.tail.head._2 - fpr_tpr.head._2) * fpr_tpr.tail.head._1)
    }

    (cumulative(fpr_tpr, 0.0), fpr_tpr)
  }

  def getPR(labels: Array[Boolean], predictions: Array[Double]): Array[(Double, Double)] = {
    require(labels.length == predictions.length, "labels and predictions should have the same length")
    val pairs: Array[(Boolean, Double)] = labels.zip(predictions)
    //val nP: Double = labels.count(_ == true).toDouble // total number of positives
    //val nN = labels.length - nP // total number of negatives
    //val P_inc = 1/nN
    //val R_inc = 1/nP

    val thresholds = pairs.map(_._2).distinct.sorted.reverse

    val pr: Array[(Double, Double)] = for {threshold <- thresholds} yield {
      val top = pairs.filter(x => x._2 >= threshold)
      val tp = top.map(_._1).count(_ == true).toDouble
      val fp = top.map(_._1).count(_ == false).toDouble
      val fn = pairs.filter(x => x._2 < threshold).map(_._1).count(_ == true).toDouble
      val precision = tp / (tp + fp)
      val recall = tp / (tp + fn)
      (precision, recall)
    }
    //pr.sortBy(_._2)
    (1.0, 0.0) +: pr
  }

  // This is in alignment with sklearn.metrics.average_precision_score and Honglei's AP
  def getAP(labels: Array[Double], predictions: Array[(Int, Double)]): Double = {
    require(labels.length == predictions.length, "labels and predictions should have the same length")
    val sortedpredictions = predictions.sortBy(-_._2)

    var AP = 0.0
    var i = 0.0
    var p = 0.0

    for {
      indices <- sortedpredictions.indices
    } {
      val label = labels(sortedpredictions(indices)._1)
      i = i + 1
      p = p + label
      AP = AP + label * (p / i)
    }
    //println(s"$i, $p, $AP")
    if (p == 0) 0
    else AP / p
  }

  def getRecallPrecisionAt125etc(labels: Array[Double], predictions: Array[(Int, Double)]): Array[(Double, Double)] = {
    val result = Array(1, 2, 5, 10, 20, 30).map { p =>
      val (top_predictions, indexes) = predictions.zipWithIndex.sortBy(-_._1._2).take(math.floor((predictions.length / 100.0) * p).toInt).unzip
      //val top_predictions = predictions.sortBy(_._2).take(math.floor((predictions.length/100.0) * p).toInt)
      //val top_labels = top_predictions.map(x => labels(x._1))
      val top_labels: Array[Double] = indexes.map(x => labels(x))
      val nTypeA = labels.count(x => x == 1.0)

      var fp = 0
      var fn = 0
      var tp = 0
      var tn = 0

      for {
        i <- top_labels.indices
      } {
        if (top_labels(i) == 1.0) { // in that case it was an outlier
          tp += 1 // and we detected it
          //if(top_predictions(i)._1 == 0) { // and we detected it
          //  tp += 1
          //} else { // and we did not detect it
          //  fn += 1
          //}
        } else { // in that case it was not an outlier
          fp += 1 // and we said it is
          //if (top_predictions(i)._1 == 0) { // and we said it is
          //  fp += 1
          //} else { // and we did not say it is
          //  tn += 1
          //}
        }
      }
      println(s"p: $p -- nOut = ${nTypeA}, tp= $tp, fp= $fp, tn= $tn, fn= $fn")
      val precision = if ((tp.toDouble + fp.toDouble) == 0) 0.0 else tp.toDouble / (tp.toDouble + fp.toDouble)
      //val recall = tp.toDouble / nTypeB.toDouble // would be small if corruption is large
      val recall = if ((tp.toDouble + fn.toDouble) == 0) 0.0 else tp.toDouble / nTypeA.toDouble //(tp.toDouble + fn.toDouble)
      (recall, precision)
    }
    result
  }
}
