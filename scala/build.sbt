name := "GildedRose"

version := "1.0"

scalaVersion := "2.13.0"

libraryDependencies := Seq(
  "eu.timepit" %% "refined" % "0.9.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
)
