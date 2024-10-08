val scala3Version = "3.3.3"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions", "-Wunused:all")

lazy val dataMapping = project
  .in(file("."))
  .settings(
    name := "data-mapping",
    organization := "com.greenfossil",
    version := "1.0.23",

    scalaVersion := scala3Version,

    Compile / javacOptions ++= Seq("-source", "17"),

    libraryDependencies ++= Seq(
      "com.greenfossil" %% "commons-json" % "1.0.17",
      "com.typesafe" % "config" % "1.4.3",
      "org.slf4j" % "slf4j-api" % "2.0.12",
      "ch.qos.logback" % "logback-classic" % "1.5.6" % Test,
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )

//https://www.scala-sbt.org/1.x/docs/Publishing.html
ThisBuild / versionScheme := Some("early-semver")

//Remove logback from test jar
Test / packageBin / mappings ~= {
  _.filterNot(_._1.getName.startsWith("logback"))
}