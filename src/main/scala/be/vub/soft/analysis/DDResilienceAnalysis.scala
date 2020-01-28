package be.vub.soft.analysis

import be.vub.soft.debugging.{AutomatedDeltaDebugging, DDNoErrorFound, DDOutcome, DDResult, DDTimeout}
import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.ActorConfig
import be.vub.soft.perturbations.Perturbation
import be.vub.soft.tracer.TestReport

class DDResilienceAnalysis(override val perturbation: Perturbation) extends ResilienceAnalysis {

    var cache: Map[Set[ActorConfig], Status] = Map() // cache subsequent tests

    def name = "DD"

    def toSubset(diff: Set[ActorConfig], n: Int): List[Set[ActorConfig]] = {
        diff.grouped(diff.size / n).toList
    }

    // Our test function
    def testFunction(f: Set[ActorConfig] => Status): (Set[ActorConfig] => Status) = (s: Set[ActorConfig]) => {
        //TODO: we did not implement monotonicity, property p14/15
        if(cache.isDefinedAt(s)) {
            cache(s)
        } else {
            val rs = f(s)
            cache = cache + (s -> rs)
            rs
        }
    }

    override def run(f: Set[ActorConfig] => Status, initReport: TestReport, start: Long, timeout: Int): Unit = {
        val r: DDOutcome = AutomatedDeltaDebugging.dda(perturbations, this, testFunction(f), start, timeout, toSubset)
        result = r match {
            case DDTimeout => None
            case DDResult(cok, cnok) => Some(cnok.diff(cok))
            case DDNoErrorFound => None
        }
    }

    override def next(): ActorConfig = ActorConfig()
    override def hasNext: Boolean = result.isEmpty
}
