name := "scala-fp-school"
organization := "io.github.ssledz"
scalaVersion := "2.13.4"

addCommandAlias("fmt", ";scalafmt ;test:scalafmt ;scalafmtSbt")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.3").cross(CrossVersion.full))
enablePlugins(ScalafmtPlugin)

