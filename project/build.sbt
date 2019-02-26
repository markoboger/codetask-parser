name := """codetask-parser"""
organization := "de.htwg-konstanz.ar431ott"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.6"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test

libraryDependencies += ws


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.3"
