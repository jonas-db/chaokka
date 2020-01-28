name := "SystemUnderTest2"

version := "0.1"

scalaVersion := "2.12.8"


resolvers += Resolver.bintrayRepo("dnvriend", "maven")

libraryDependencies += "commons-io" % "commons-io" % "2.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.23"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.5.23"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.23" % Test
libraryDependencies += "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.15.2"
libraryDependencies += "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"