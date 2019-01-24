name := "Code-Task Parser"
organization := "de.htwg-konstanz"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.3"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"