lazy val commonSettings = Seq(
  // organization := "com.example",
  version := "0.1.0"
  // scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
  dependsOn(ProjectRef(uri("https://github.com/jtfmumm/digit-parser"), "digit-parser")).
  settings(commonSettings: _*).
  settings(
    name := "Set Theory"
  )