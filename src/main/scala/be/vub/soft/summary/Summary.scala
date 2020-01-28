package be.vub.soft.summary

import java.io.{File, PrintWriter}
import java.nio.file.Paths

import be.vub.soft.parser.ActorConfig

case class RTResult(n: Int, iterations: Int, isCorrect: Boolean, unresolved: Boolean, time: Int, timeout: Boolean, initialDuration: Long, averagePerturbedDuration: Long, topology: Int, nodes: Int, edges: Int, faultType: String, fault: String, technique: String, result: Set[ActorConfig])

class Summary(suite: String, test: String, analysis: String, private var results: List[RTResult] = List.empty[RTResult]) {
    var perturbations = 0
    def setPerturbations(totalPerturbations: Int) = perturbations = totalPerturbations

    def toCSV(outDir: File): Unit = {
        val errorOccurred = results.exists(_.isCorrect == false)
        val outFileCSV: String = Paths.get(outDir.getAbsolutePath, s"$suite.$test-$perturbations-$analysis-$errorOccurred.csv").toString
        val outFileAll: String = Paths.get(outDir.getAbsolutePath, s"$suite.$test-$perturbations-$analysis-$errorOccurred.txt").toString

        val header = "Run,Iterations,Correct,Unresolved,Time,Timeout,InitialDuration,AveragePerturbedDuration,Topology,Nodes,Edges,Perturbations,FaultType,Fault,Technique" + "\n"

        val contentsCSV: String = s"$header${results.map(r => s"${r.n},${r.iterations},${r.isCorrect},${r.unresolved},${r.time},${r.timeout},${r.initialDuration},${r.averagePerturbedDuration},${r.topology},${r.nodes},${r.edges},${perturbations},${r.faultType},${r.fault},${r.technique}").mkString("\n")}"
        val contentsAll: String = s"$header${results.map(r => s"${r.n},${r.iterations},${r.isCorrect},${r.unresolved},${r.time},${r.timeout},${r.initialDuration},${r.averagePerturbedDuration},${r.topology},${r.nodes},${r.edges},${perturbations},${r.faultType},${r.fault},${r.technique},${r.result}").mkString("\n")}"

        new PrintWriter(outFileCSV) { write(contentsCSV); close() }
        new PrintWriter(outFileAll) { write(contentsAll); close() }
    }

    def append(result: RTResult): Unit = {
        results = result :: results
    }
}
