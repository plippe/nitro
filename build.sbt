scalaVersion := "2.12.6"

scalafmtOnCompile := true

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.2.0",
    "org.specs2" %% "specs2-core" % "4.2.0" % Test
)
