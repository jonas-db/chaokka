package be.vub.soft.analysis

import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.{ActorConfig, JSONParser}
import be.vub.soft.perturbations.Perturbation
import be.vub.soft.tracer.TestReport

class StaticResilienceAnalysis(configFilePath: String, override val perturbation: Perturbation) extends ResilienceAnalysis {

    perturbations = JSONParser.load(configFilePath, 0).toSet

    override def run(f: Set[ActorConfig] => Status, initialReport: TestReport, start: Long, timeout: Int): Unit = {
        f(perturbations)
    }

    override def hasNext: Boolean = false

    override def name: String = "static"
}
