enablePlugins(ScalaJSPlugin)

name := "Pendulum"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "org.scala-js" %%% "scalajs-java-time" % "0.2.3"
)
