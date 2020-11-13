name := "SGMRD"
organization := "io.github.edouardfouche"

version := "0.1.0"

scalaVersion := "2.12.8"
crossScalaVersions := Seq("2.11.8", "2.12.8") // prefix with "+" to perform for both .e.g, "+ compile"

//javaOptions += "-Xmx30G"
//javaOptions += "-Xms10G"

fork in run := true
scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies += "de.lmu.ifi.dbs.elki" % "elki" % "0.7.5"
libraryDependencies += "de.lmu.ifi.dbs.elki" % "elki-logging" % "0.7.5"
libraryDependencies += "de.lmu.ifi.dbs.elki" % "elki-index-various" % "0.7.5"
libraryDependencies += "de.lmu.ifi.dbs.elki" % "elki-index-rtree" % "0.7.5"
libraryDependencies += "de.lmu.ifi.dbs.elki" % "elki-input" % "0.7.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "commons-io" % "commons-io" % "2.6"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

libraryDependencies += "org.jzy3d" % "jzy3d-api" % "1.0.0" //from "http://maven.jzy3d.org/releases/"
resolvers += "Jzy3d Maven Release Repository" at "http://maven.jzy3d.org/releases"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.1",
  "org.scalanlp" %% "breeze-natives" % "0.13.1"
)

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

javacOptions ++= Seq("-encoding", "UTF-8")