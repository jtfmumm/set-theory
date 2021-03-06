lazy val digitParser = "com.jtfmumm" % "digit-parser_2.10" % "0.1.1"

lazy val commonSettings = Seq(
  organization := "com.jtfmumm",
  version := "0.1.4"
  // scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Set Theory",
    libraryDependencies += digitParser,
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomExtra := (
      <url>http://github.com/jtfmumm/set-theory</url>
        <licenses>
          <license>
            <name>MIT</name>
            <url>http://opensource.org/licenses/MIT</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:jtfmumm/set-theory.git</url>
          <connection>scm:git:git@github.com:jtfmumm/set-theory.git</connection>
        </scm>
        <developers>
          <developer>
            <id>jtfmumm</id>
            <name>John Mumm</name>
            <url>http://jtfmumm.com</url>
          </developer>
        </developers>)
  )