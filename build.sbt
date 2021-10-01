val scala3Version     = "3.0.2"
val Http4sVersion     = "1.0.0-M23"
val CirceVersion      = "0.15.0-M1"
val CatsEffectVersion = "3.2.8"
val Fs2Version        = "3.1.2"
val DoobieVersion     = "1.0.0-RC1"
val ScalaTestVersion  = "3.2.10"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "scala3-simple",
    version      := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"        % CatsEffectVersion,
      "co.fs2"        %% "fs2-core"           % Fs2Version,
      "org.tpolecat"  %% "doobie-core"        % DoobieVersion,
      "org.tpolecat"  %% "doobie-postgres"    % DoobieVersion,
      "org.scalactic" %% "scalactic"          % ScalaTestVersion,
      "org.scalatest" %% "scalatest"          % ScalaTestVersion % "test",
      "org.scalatest" %% "scalatest-funsuite" % ScalaTestVersion % "test"
    )
  )
