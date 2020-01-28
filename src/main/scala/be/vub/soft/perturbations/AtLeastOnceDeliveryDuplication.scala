package be.vub.soft.perturbations
import be.vub.soft.Main
import be.vub.soft.parser.{ActorConfig, ActorMessage}
import be.vub.soft.tracer.{SendV2, TestReport, Traceable}

/*
    Inject a duplicated message to check against faults in resilient systems against message failures
 */

object AtLeastOnceDeliveryDuplication extends Perturbation {

    override def pre(perturbable: Traceable, report: TestReport): Boolean = perturbable match {
        case s: SendV2 if s.spath.contains("/user/") && s.rpath.contains("/user/") && report.atLeastOnceDeliveryMessages.contains(s.clazz) => true
        case _ => false
    }

    override def inject[SendV2](perturbable: SendV2, report: TestReport, messageCandidates: Set[Traceable], actorCandidates: Set[Traceable]): List[ActorConfig] = perturbable match {
        case SendV2(spath, _, rpath, _, hash, clazz, _, _) =>
            val actorName = rpath // Receiving actor
            val senderName = spath
            val message = ActorMessage(clazz.r, hash, 0, 1, 0, senderName.r, ".*".r, ".*".r)
            val messages = List(message)

            val config = ActorConfig(actorName.r, ".*".r, ".*".r, 0.0, messages)

            //println(s"AtLeastOnceDeliveryDuplication: \n\t$perturbable\n\t$config")

            List(config)
    }
}
