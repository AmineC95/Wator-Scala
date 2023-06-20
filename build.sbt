ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "Wator",
    libraryDependencies +="org.scalafx" %% "scalafx" % "20.0.0-R31"
  )
