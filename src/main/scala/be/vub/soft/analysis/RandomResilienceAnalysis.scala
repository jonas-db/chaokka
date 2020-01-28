package be.vub.soft.analysis

import be.vub.soft.debugging.Status
import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.ActorConfig
import be.vub.soft.perturbations.Perturbation
import be.vub.soft.tracer.TestReport

class RandomResilienceAnalysis(override val perturbation: Perturbation) extends ResilienceAnalysis {

     override def run(f: Set[ActorConfig] => Status, initialReport: TestReport, start: Long, timeout: Int): Unit = {
        val n: Set[ActorConfig] = Set(next())
        val r: Status = f(n)

        if(r == Status.Fail) {
            perturbations = Set.empty[ActorConfig]
            result = Some(n)
        }
    }

    def name = "random"
}
