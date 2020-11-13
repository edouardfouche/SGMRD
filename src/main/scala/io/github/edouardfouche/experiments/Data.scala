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

import io.github.edouardfouche.preprocess.DataRef

object Data {
  lazy val pyro: DataRef = DataRef("b_1dx100_s", currentdir + "/data/real/pyro.csv", 1, ",", "real")
  lazy val kddcup99: DataRef = DataRef("kdd99", currentdir + "/data/real/kddcup99.csv", 1, ",", "RSHash")
  lazy val activity: DataRef = DataRef("activity", currentdir + "/data/real/activity.csv", 1, ",", "RSHash")
  lazy val example_10: DataRef = DataRef("example_10", currentdir + "/data/synthetic/example_10_data.csv", 1, ",", "synth")
  lazy val example_20: DataRef = DataRef("example_20", currentdir + "/data/synthetic/example_20_data.csv", 1, ",", "synth")
  lazy val example_50: DataRef = DataRef("example_50", currentdir + "/data/synthetic/example_50_data.csv", 1, ",", "synth")
  val currentdir: String = System.getProperty("user.dir")
  val home: String = System.getProperty("user.home")

  // helper function to get the relative path to resources
  def fetch(path: String): String = try {
    val fullpath = currentdir + "/data" + path
    val s = scala.io.Source.fromFile(fullpath)
    s.close()
    fullpath
  } catch {
    case _: Throwable => {
      try {
        val fullpath2 = currentdir + "/git/data" + path
        val s = scala.io.Source.fromFile(fullpath2)
        s.close()
        fullpath2
      } catch {
        case _: Throwable => {
          val message = s"Tried ${currentdir + "/data" + path} and ${currentdir + "/git/data" + path}"
          throw new Error(message)
        }
      }
    }
  }
}
