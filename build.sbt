
name := "hello-doobie-shapeless"

scalaVersion := "2.11.11"


val doobieVersion = "0.4.2"

libraryDependencies ++= Seq(
 		   "org.hsqldb" % "hsqldb" % "2.3.4" % Test,
    		   "org.tpolecat" %% "doobie-core-cats"       % doobieVersion,
		   "org.tpolecat" %% "doobie-hikari-cats"     % doobieVersion,
		   "org.tpolecat" %% "doobie-postgres-cats"   % doobieVersion,
    		   "org.tpolecat" %% "doobie-specs2-cats"     % doobieVersion)