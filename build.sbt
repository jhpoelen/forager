lazy val root = (project in file(".")).
  settings(
    name := "foragus",
    version := "1.0",
    scalaVersion := "2.11.4"
  )

resolvers ++= Seq(
  "anormcypher" at "http://repo.anormcypher.org/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)


libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.anormcypher" %% "anormcypher" % "0.6.0"
)