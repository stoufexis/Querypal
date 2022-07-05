val scala3Version     = "3.1.3"
val CatsEffectVersion = "3.2.8"
val DoobieVersion     = "1.0.0-RC1"
val ScalaTestVersion  = "3.2.10"

//githubOwner      := "StefanosTouf"
//githubRepository := "Querypal"
//githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource
//  .Environment("GITHUB_TOKEN")

lazy val root = project
  .in(file("."))
  .settings(
    name         := "Querypal",
    organization := "StefanosTouf",
    version      := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"        % CatsEffectVersion,
      "org.tpolecat"  %% "doobie-core"        % DoobieVersion,
      "org.tpolecat"  %% "doobie-postgres"    % DoobieVersion,
      "org.scalactic" %% "scalactic"          % ScalaTestVersion,
      "org.scalatest" %% "scalatest"          % ScalaTestVersion % "test",
      "org.scalatest" %% "scalatest-funsuite" % ScalaTestVersion % "test"
    )
  )
