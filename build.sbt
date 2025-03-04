ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "FINALOCHKA"
  )

// https://mvnrepository.com/artifact/org.typelevel/cats-effect
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6-0142603"
// https://mvnrepository.com/artifact/com.softwaremill.sttp.client4/core
libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M26"
// https://mvnrepository.com/artifact/com.softwaremill.sttp.client4/cats
libraryDependencies += "com.softwaremill.sttp.client4" %% "cats" % "4.0.0-M26"


libraryDependencies +=  "com.github.pureconfig" %% "pureconfig-core"           % "0.17.8"
libraryDependencies +=  "com.github.pureconfig" %% "pureconfig-cats-effect"    % "0.17.8"
libraryDependencies +=  "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.8"

val circeVersion = "0.14.10"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)