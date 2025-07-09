val scala3Version = "3.7.1"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions", "-Wunused:all")

lazy val dataMapping = project
  .in(file("."))
  .settings(
    name := "data-mapping",
    organization := "com.greenfossil",
    version := "1.3.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.greenfossil" %% "commons-json" % "1.3.0",
      "com.typesafe" % "config" % "1.4.3",
      "org.slf4j" % "slf4j-api" % "2.0.16",
      "ch.qos.logback" % "logback-classic" % "1.5.16" % Test,
      "org.scalameta" %% "munit" % "1.1.1" % Test
    )
  )

//https://www.scala-sbt.org/1.x/docs/Publishing.html
ThisBuild / versionScheme := Some("early-semver")
