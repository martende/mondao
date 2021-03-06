organization := "com.github.martende"

name := "mondao"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.mongodb.scala" % "mongo-scala-driver_2.11" % "1.1.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0-M16-SNAP6" % Test

libraryDependencies += "joda-time" % "joda-time" % "2.1"