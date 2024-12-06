ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "Proof-Generator-CSCI3434",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
  )
