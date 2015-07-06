lazy val commonSettings = Seq(
  // organization := "com.example",
  version := "0.1.0"
  // scalaVersion := "2.11.4"
)

lazy val gitRepo = "git:https://github.com/jtfmumm/digit-parser#master"

lazy val digitParser = RootProject(uri(gitRepo))

lazy val root = (project in file(".")).
  dependsOn(digitParser).
  settings(commonSettings: _*).
  settings(
    name := "Set Theory"
  )