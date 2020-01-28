package be.vub.soft.runner

import java.io.{File, FileInputStream, FileWriter, ObjectInputStream}
import java.nio.file.Paths

import be.vub.soft.Main
import be.vub.soft.analysis.ResilienceAnalysis
import be.vub.soft.debugging.Status
import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.{ActorConfig, JSONParser}
import be.vub.soft.perturbations.{AtLeastOnceDeliveryDuplication, PersistentActorRestart, Perturbation}
import be.vub.soft.reporter.DiscoveredTest
import be.vub.soft.summary.{RTResult, Summary}
import be.vub.soft.topology.Topology
import be.vub.soft.tracer.{ActorRegistration, ActorTurn, Send, TestReport, Traceable}
import be.vub.soft.utils.Utils
import org.apache.commons.io.FileUtils

import scala.io.Source
import scala.sys.process.{Process, ProcessLogger}

class ResilienceAnalysisRunner(target: String, suite: Option[String] = None, test: Option[String] = None) {

    /*
        General options
     */
    val short           = true // Debugging, true -> only message type, false -> complete message
    val skipTrace       = true

    /*
        Modifiable options
     */
    val systemName      = target.substring(target.lastIndexOf("/"))
    val config          = Paths.get(target, "perturbation.json").toAbsolutePath.toString
    val testClasses     = Paths.get(target, "target", "scala-2.12", "test-classes").toAbsolutePath.toString
    //val outFolder       = Paths.get("./", systemName).toFile
    val csvFolder       = Paths.get("", "csv").toFile

    /*
        Should not be modified
     */
    val index               = Paths.get(target, "tests.index").toFile
    val perturbationFolder  = Paths.get(target, s"output").toAbsolutePath.toString
    val pwd                 = Paths.get(target).toFile
    val lib                 = Paths.get(target, "lib").toFile
    val plugins             = Paths.get(target, "project", "plugins.sbt").toFile
    val pluginPath          = Paths.get(target, "lib", "sbt-aspectj.jar")
    val plugin              = s"""
                                |libraryDependencies += "org.aspectj" % "aspectjtools" % "1.8.10"
                                |addSbtPlugin("com.lightbend.sbt" % "sbt-aspectj" % "0.11.1" from "file://$pluginPath")
                               """.stripMargin

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//    if(!outFolder.exists()) outFolder.mkdir()
//    FileUtils.cleanDirectory(outFolder)

    if(!csvFolder.exists()) csvFolder.mkdir()
    FileUtils.cleanDirectory(csvFolder)

    val perturbationFolderFile: File = new File(perturbationFolder)
    if(!perturbationFolderFile.exists()) perturbationFolderFile.mkdir()
    FileUtils.cleanDirectory(perturbationFolderFile)

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    println(s"Copying local jar files")

    if(!lib.exists()) {
        lib.mkdirs()
    }

    val jars = Paths.get("./jars").toFile
    println(s"jars found: ${jars.exists()}")
    if(!jars.exists()) {
        println("Missing jar files: they should be in the folder 'jars' located in the same folder as the tool.jar")
        System.exit(0)
    }

    FileUtils.copyDirectory(jars, lib)

    println(s"Creating plugins.sbt")

    if(!plugins.exists()) {
        plugins.createNewFile()
    }

    val file = Source.fromFile(plugins.getAbsolutePath)
    val exists = file.getLines.exists(l => l.contains("sbt-aspectj"))

    if(!exists) {
        val fw = new FileWriter(plugins.getAbsolutePath, true)
        try fw.write(plugin) finally fw.close()
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val nullLogger = ProcessLogger(line => (), line => ())

    def initialize(): Unit = {
        val exists = index.exists()

        if(!exists) {
            println("Discovering tests...")
            val processSbtDiscover = Process(Seq("sbt", s"discover $testClasses"), pwd)
            val processSbtDiscoverCode = processSbtDiscover ! nullLogger

            if(processSbtDiscoverCode == 0) {
                println("Discovering tests successfully done.")
            } else {
                println("Unable to discover tests. Exiting...")
                System.exit(0)
            }
        }

        // execute sbt test to make sure latest changes are compiled
//        val processSbtTest = Process(Seq("sbt", "test"), pwd)
//        processSbtTest ! initialLogger
//
//        println("processSbtTest")
    }

    def readIndex(): Map[String, List[DiscoveredTest]] = if(index.exists()) {
        val f = new FileInputStream(index)
        val s = new ObjectInputStream(f)
        val mapping: Map[String, List[DiscoveredTest]] = s.readObject().asInstanceOf[Map[String, List[DiscoveredTest]]]
        s.close()

        mapping
    } else Map.empty[String, List[DiscoveredTest]]

    def writePerturbations(output: String, test: String, suite: String, n: Int, s: Set[ActorConfig]): String = {
        val localConfigPath = Paths.get(output, s"${test.hashCode}-${suite.hashCode}-$n-config.json").toFile.getAbsolutePath
        JSONParser.writeSet(localConfigPath, s)

        // Give it a bit of time to write to the file, probably unnecessary
        Thread.sleep(1000) //TODO: while true wait for exists

        localConfigPath
    }

    def getReport(run: Int, n: Int, test: String, suite: String): Option[TestReport] = {
        // Give it some time before reading, this might avoid EOFFexcepetions when reading the report?
        Thread.sleep(1000)

        val perturbationFolderForRun = Paths.get(perturbationFolder, s"$run").toAbsolutePath.toString
        val p = Paths.get(perturbationFolderForRun, s"${test.hashCode}-${suite.hashCode}-$n.bin").toFile

        try {
            if (p.exists()) {
                val read = be.vub.soft.tracer.Tracer.read(p.getAbsolutePath) // this failed once throwing an EOFexception..?
                Some(read)
            } else {
                None
            }
        } catch {
            case e: Exception => None
        }
    }

    def checkStatus(x: TestReport): Status = if(x.unappliedPerturbations.isEmpty) {
        if (x.success) {
            Status.Pass
        } else {
            //TODO: if current error is the same as first error, then Fail otherwise Unresolved
            Status.Fail
        }
    } else Status.Unresolved /* if not all perturbations are applied we don't know anything */

    def readStatus(run: Int, n: Int, test: String, suite: String): Status = {
        val report = getReport(run, n, test, suite)
        val status = report.map(x => checkStatus(x)).getOrElse(Status.Unresolved)
        status
    }

    def getUnappliedPerturbations(run: Int, n: Int, test: String, suite: String): Int = {
        val report = getReport(run, n, test, suite)
        val status = report.map(x => x.unappliedPerturbations.size).getOrElse(-1)
        status
    }

    def filterSuite(mapping: Map[String, List[DiscoveredTest]]): Map[String, List[DiscoveredTest]] = suite match {
        case Some(suite) => mapping.filter(ts => ts._1.contains(suite))
        case None => mapping
    }

    def filterTest(mapping: Map[String, List[DiscoveredTest]]): Map[String, List[DiscoveredTest]] = test match {
        case Some(test) => mapping.map(x => (x._1, x._2.filter(tc => tc.name.contains(test))))
        case None => mapping
    }

    def logError(test: String, ex: String) = {
        val fw = new FileWriter(Paths.get("", "errors.txt").toAbsolutePath.toString, true)
        try {
            val str = s"Exception for $test:\n$ex\n"
            println(str)
            fw.write(str)
        }
        finally fw.close()
    }

    def analyse(spawnAnalysis: (DiscoveredTest) => ResilienceAnalysis, runs: Int = 1, timeout: Int = Main.TIMEOUT): Unit = {

        println("Starting analysis...")
        println(s"CSV Folder: ${csvFolder.getAbsolutePath}")
        println(s"Output Folder: ${perturbationFolder}")

        // Read index
        val mapping = filterTest(filterSuite(readIndex()))

        // Print mapping
        mapping.foreach({
            case (k, v) =>
                println(k)
                v.foreach(t => println(s"    ${t.name} (${t.succeeds}, ${t.duration}ms)"))
        })

        // For every test suite
        for (((suite, tests), suiteID) <- mapping.zipWithIndex) {

            // For every test case
            for ((tuple, testID) <- tests.zipWithIndex) { // .reverse
                val DiscoveredTest(test, succeeded, duration, cmdl) = tuple
                val cmd: List[String] = List(cmdl.head, "-Xms8g", "-Xmx8g") ++ cmdl.tail

                // Only execute initially succeeded tests
                if (succeeded) {

                    val technique = spawnAnalysis(tuple).name
                    val summary = new Summary(suite, test, technique)

                    println(s"Analysing $test using '$technique'")

                    // Number of runs
                    for (run <- 1 to runs) {
                        try {
                            val analyzer = spawnAnalysis(tuple)
                            val perturbationFolderForRun = Paths.get(perturbationFolder, s"$run").toAbsolutePath.toString

                            println(s"Running initial iteration for '$test'")

                            val initial = execute(test, suite, 0, cmd, "", perturbationFolderForRun)
                            var steps: List[(Status, Set[String])] = List.empty

                            if (initial == 0) {
                                val initialReport = getReport(run, 0, test, suite)
                                analyzer.onInitialReport(initialReport)
                                val initialDuration: Long = initialReport.map(r => r.duration).getOrElse(-1)

                                println("Initial iteration successful")

                                val start = System.currentTimeMillis()
                                var iterations = 0
                                val totalPerturbations = analyzer.perturbations.size
                                summary.setPerturbations(totalPerturbations)

                                println(s"Found ${totalPerturbations} perturbations") // (expected: $predictedPerturbations)")

                                var averageDuration: Long = 0

                                val f: Set[ActorConfig] => Status = (s: Set[ActorConfig]) => {
                                    iterations = iterations + 1

                                    // Write new perturbations
                                    val localConfigPath = writePerturbations(perturbationFolderForRun, test, suite, iterations, s)

                                    // Execute the perturbations, ignore exitcode because it indicates test succeeded or test failed
                                    val exitCode = execute(test, suite, iterations, cmd, localConfigPath, perturbationFolderForRun)

                                    // On subsequent report
                                    val subsequentReport = getReport(run, iterations, test, suite)
                                    analyzer.onSubsequentReport(subsequentReport)

                                    // Compute duration with perturbations
                                    val perturbedDuration: Long = subsequentReport.map(_.duration).getOrElse(0)
                                    averageDuration = averageDuration + perturbedDuration

                                    // Read the result
                                    val rs: Status = readStatus(run, iterations, test, suite)

                                    steps = (rs, s.map(x => {
                                        val sndr = x.messages.head.senderName.toString()
                                        val rec = x.actorName.toString()
                                        s"${sndr}->${rec}"
                                    })) :: steps

                                    // Return
                                    rs
                                }

                                while (analyzer.hasNext && System.currentTimeMillis() - start < timeout) {
                                    analyzer.run(f, initialReport.get, start, timeout)
                                }

                                //println(s"Steps: ${steps.size}")
                                //println(steps.reverse.mkString("\n"))

                                val outOfTime = System.currentTimeMillis() - start >= timeout
                                val totalTimeInSeconds = if(outOfTime) timeout/1000 else ((System.currentTimeMillis() - start) / 1000).toInt
                                val outcome = analyzer.result

                                val isCorrect = true

                                val avgIteration = averageDuration / iterations
                                val realFaultType = if(analyzer.perturbation.getClass.toString.contains("At")) "a" else "p"

                                summary.append(RTResult(run, iterations, isCorrect, false, totalTimeInSeconds, outOfTime, initialDuration, avgIteration, -1, -1, -1, realFaultType, "NaN", technique, outcome.getOrElse(Set())))

                                println(s"${analyzer.name}: Run #$run ended in $iterations iterations, found=$isCorrect, ${if(analyzer.name.startsWith("DD")) s"noErrorFound=${analyzer.exception.isEmpty}" else ""} ($totalTimeInSeconds seconds, timeout=$outOfTime).\nIssue:${outcome.getOrElse(ActorConfig())}")
                            } else {
                                println(s"Cancelling test: '$test' in $suite (initial trace failed)")
                            }

                        } catch {
                            case e: Exception => logError(test, e.getMessage + ":" + e.getStackTrace.toList.mkString("\n"))
                        }
                    }
                    println(s"Resilience testing ended for '$test' in $suite (runs=$runs)")

                    // Write summary
                    summary.toCSV(csvFolder)

                    // Clean
                    FileUtils.cleanDirectory(new File(perturbationFolder))
                } else {
                    println(s"Skipping failed test: '$test' in $suite")
                }
            }
        }

        // Analysis done, write files
        println(s"Analysis finished")
    }

    protected def execute(test: String, suite: String, n: Int, cmd: List[String], localConfigPath: String, perturbationOutputFolder: String,
                          logger: ProcessLogger = ProcessLogger(line => println(line), line => println(line))): Int = {
        val process = Process(
            cmd ++ Seq("-s", suite, "-t", test),
            None,
            "PERTURBATION_CONFIGURATION" -> localConfigPath,
            "PERTURBATION_ITERATION" -> n.toString,
            "PERTURBATION_OUTPUT" -> perturbationOutputFolder,
            "PERTURBATION_FORMAT" -> short.toString,
            "PERTURBATION_SKIP_TRACE" -> skipTrace.toString)

        process ! logger
    }

}
