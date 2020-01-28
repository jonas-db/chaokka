package be.vub.soft.analysis

import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.ActorConfig
import be.vub.soft.perturbations.Perturbation
import be.vub.soft.topology.CausalityGraph
import be.vub.soft.tracer.{ActorRegistration, ActorTurn, SendV2, TestReport, Traceable, TurnV2}

import scala.util.Random

abstract class ResilienceAnalysis {

    var perturbations: Set[ActorConfig] = Set.empty[ActorConfig]
    var result: Option[Set[ActorConfig]] = None
    val perturbation: Perturbation // type of perturbation, TODO: currently one at a time
    var lastReport: Option[TestReport] = None
    var initialReport: Option[TestReport] = None
    var exception: Option[String] = None

    def isUserActor(a: String): Boolean = a.contains("/user/")
    def isAkkaMessage(a: String): Boolean = a.contains("akka")

    def onInitialReport(r: Option[TestReport]): Unit = r match {
        case Some(report) =>

            initialReport = r

            // All user messages
            val messageCandidates = report.trace.collect({
                case s: SendV2 if isUserActor(s.spath) && isUserActor(s.rpath) => s
            })

            // All user actors
            val actorCandidates = report.trace.collect({
                case a: ActorRegistration if isUserActor(a.childPath) => a
            })

            // Apply an optional filter here
            val targets = onInitialFilter(messageCandidates ++ actorCandidates)

            // Extract only the relevant messages
            val filteredMessages: List[SendV2] = targets.collect({
                case s: SendV2  => s
            })

            // Extract only the relevant actors
            val filteredActors: List[ActorRegistration] = targets.collect({
                case a: ActorRegistration  => a
            })

            //println(s"targets:\n${targets.mkString("\n")}")

            // Random shuffle perturbations
            perturbations = Random.shuffle(targets.flatMap(c => Perturbation.checkSpecific(perturbation,c, report, filteredMessages.toSet, filteredActors.toSet))).toSet[ActorConfig]

            //println(s"Perturbations:\n${perturbations.mkString("\n")}")
        case None => assert(assertion = false, "failed initial report")
    }

    def run(f: Set[ActorConfig] => Status, initialReport: TestReport, start: Long, timeout: Int): Unit

    def next(): ActorConfig = {
        val head = perturbations.head
        perturbations = perturbations - head

        head
    }

    def hasNext: Boolean = perturbations.nonEmpty

    def onSubsequentReport(report: Option[TestReport]): Unit = {
        if(report.nonEmpty && report.exists(r => r.iteration == 1)) {
            exception = Some(report.get.exception)
        }

        lastReport = report
    }

    def onInitialFilter(targets: List[Traceable]): List[Traceable] = targets.filter({
        case s: SendV2 => !isAkkaMessage(s.clazz) // We do not want to perturb akka native messages
        case _ => true
    })

    def name: String
}
