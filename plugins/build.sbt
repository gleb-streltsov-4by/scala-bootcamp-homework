ThisBuild / version := "1.0.0"
ThisBuild / organization := "com.bootcamp"
ThisBuild / scalaVersion := "2.12.12"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "plugins",
  )