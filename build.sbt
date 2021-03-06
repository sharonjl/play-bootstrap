name := """play-bootstrap"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "RoundEights" at "http://maven.spikemark.net/roundeights"
)

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.postgresql" % "postgresql" % "9.3-1102-jdbc41",
  "com.roundeights" %% "hasher" % "1.0.0",
  "org.scalatestplus" %% "play" % "1.2.0" % "test",
  "com.github.nscala-time" %% "nscala-time" % "1.8.0"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)