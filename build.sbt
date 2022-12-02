val scala3Version = "3.2.0"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions")

lazy val commonsJsonVersion="1.0.0"

lazy val dataMapping = project
  .in(file("."))
  .settings(
    name := "data-mapping",
    organization := "com.greenfossil",
    version := "1.0.0",

    scalaVersion := scala3Version,

    Compile / javacOptions ++= Seq("-source", "17"),

    libraryDependencies ++= Seq(
      "com.greenfossil" %% "commons-json" % commonsJsonVersion,
      "com.typesafe" % "config" % "1.4.2",
      "org.slf4j" % "slf4j-api" % "2.0.3",
      "ch.qos.logback" % "logback-classic" % "1.4.4" % Test,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )

