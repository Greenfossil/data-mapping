val scala3Version = "3.3.0"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions", "-Wunused:all")

lazy val dataMapping = project
  .in(file("."))
  .settings(
    name := "data-mapping",
    organization := "com.greenfossil",
    version := "1.0.5-RC2",

    scalaVersion := scala3Version,

    Compile / javacOptions ++= Seq("-source", "17"),

    libraryDependencies ++= Seq(
      "com.greenfossil" %% "commons-json" % "1.0.4-RC1",
      "com.typesafe" % "config" % "1.4.2",
      "org.slf4j" % "slf4j-api" % "2.0.5",
      "ch.qos.logback" % "logback-classic" % "1.4.7" % Test,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )

