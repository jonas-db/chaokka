package be.vub.soft.analysis

import be.vub.soft.debugging.Status
import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.ActorConfig
import be.vub.soft.perturbations.Perturbation
import be.vub.soft.tracer.TestReport

class OverheadAnalysis(n: Int, override val perturbation: Perturbation) extends ResilienceAnalysis {

    println(s"Starting $name")

    override def run(f: Set[ActorConfig] => Status, initialReport: TestReport, start: Long, timeout: Int): Unit = {
        val set: Set[ActorConfig] = perturbations.take(n)
        perturbations = Set()
        val r: Status = f(set)
    }

    def name = s"overhead_$n"
}
