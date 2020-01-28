name := "PerturbationTool"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "commons-io" % "commons-io" % "2.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.23"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.5.23"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.23" % Test
libraryDependencies ++= {
    val akka      = "com.typesafe.akka"
    val akkaV     = "2.5.23"
    val akkaHttpV = "10.1.10"
    val circe     = "io.circe"
    val circeV    = "0.11.1"
    Seq(
        akka  %% "akka-stream"        % akkaV,
        akka  %% "akka-http"          % akkaHttpV,
        circe %% "circe-core"         % circeV,
        circe %% "circe-generic"      % circeV,
        circe %% "circe-parser"       % circeV
    )
}

libraryDependencies += "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"
unmanagedJars in Compile += file("./jars/PerturbationAspects-assembly-0.1.0-SNAPSHOT.jar")

fork in run := true