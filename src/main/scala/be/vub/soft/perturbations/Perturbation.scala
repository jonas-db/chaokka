package be.vub.soft.perturbations

import be.vub.soft.parser.ActorConfig
import be.vub.soft.tracer.{SendV2, TestReport, Traceable}

object Perturbation {

    val perturbations: List[Perturbation] = List(
        AtLeastOnceDeliveryDuplication,
        PersistentActorRestart,
        //AtLeastOnceDeliveryDelay,
        //ActorRestart
    )

    def check(traceable: Traceable, report: TestReport, messageCandidates: Set[SendV2], actorCandidates: Set[Traceable]): List[ActorConfig] = perturbations.flatMap({
        case p if p.pre(traceable, report) => p.inject(traceable, report, messageCandidates.asInstanceOf[Set[Traceable]], actorCandidates)
        case _ => List.empty[ActorConfig]
    })

    def checkSpecific(p: Perturbation, traceable: Traceable, report: TestReport, messageCandidates: Set[SendV2], actorCandidates: Set[Traceable]): List[ActorConfig] = if(p.pre(traceable, report)) {
        p.inject(traceable, report, messageCandidates.asInstanceOf[Set[Traceable]], actorCandidates)
    } else List.empty[ActorConfig]

}

abstract class Perturbation {

    def pre(perturbable: Traceable, report: TestReport): Boolean
    def inject[A <: Traceable](perturbable: A, report: TestReport, messageCandidates: Set[Traceable], actorCandidates: Set[Traceable]): List[ActorConfig]

}
