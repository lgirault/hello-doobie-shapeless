
name := "hello-doobie-shapeless"

scalaVersion := "2.11.11"


val doobieVersion = "0.4.2"

libraryDependencies ++= Seq(
  "org.hsqldb" % "hsqldb" % "2.3.4" % Test,
  "org.tpolecat" %% "doobie-core-cats" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari-cats" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres-cats" % doobieVersion,
  "org.tpolecat" %% "doobie-specs2-cats" % doobieVersion)

val scalaTestVersion = "3.0.3"

val scalaCheckDependencies =
  Seq("org.scalatest" %% "scalatest" % scalaTestVersion,
    "org.scalacheck" %% "scalacheck" % "1.13.4",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.7")

val testDependencies: Seq[ModuleID] = scalaCheckDependencies map (_ % Test)


libraryDependencies ++= testDependencies

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false