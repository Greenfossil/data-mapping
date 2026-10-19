val scala3Version = "3.8.3"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions", "-Wunused:all")

lazy val dataMapping = project
  .in(file("."))
  .settings(
    name := "data-mapping",
    organization := "com.greenfossil",
    version := "1.4.1-RC1",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.greenfossil" %% "commons-json" % "1.5.0-RC1",
      "com.typesafe" % "config" % "1.4.9",
      "org.slf4j" % "slf4j-api" % "2.0.20",
      "org.jsoup" % "jsoup" % "1.23.2",
      "com.googlecode.owasp-java-html-sanitizer" % "owasp-java-html-sanitizer" % "20260313.1",
      "ch.qos.logback" % "logback-classic" % "1.6.5" % Test,
      "org.scalameta" %% "munit" % "1.3.6" % Test
    )
  )

//https://www.scala-sbt.org/1.x/docs/Publishing.html
ThisBuild / versionScheme := Some("early-semver")
