package be.vub.soft.perturbations
import be.vub.soft.tracer.{ActorRegistration, Send, SendV2, TestReport, Traceable}
import be.vub.soft.Main
import be.vub.soft.parser.{ActorConfig, ActorMessage}
import be.vub.soft.utils.Constants

/*
    Inject an actor restart (e.g. due to a node going down)
 */

object PersistentActorRestart extends Perturbation {

    override def pre(traceable: Traceable, report: TestReport): Boolean = traceable match {
        case a: ActorRegistration => a.inheritance.contains(Constants.PersistentActor) || a.inheritance.contains(Constants.AtLeastOnceDelivery)
        case _ => false
    }

    override def inject[ActorRegistration](perturbable: ActorRegistration, report: TestReport, messageCandidates: Set[Traceable], actorCandidates: Set[Traceable]): List[ActorConfig] = perturbable match {
        case ActorRegistration(_, _, _, path, _, _, rtype, _, _) =>

        val config = messageCandidates.asInstanceOf[Set[SendV2]].toList.filter(x => !x.clazz.startsWith("akka") && x.rpath.equals(path)).map(x =>
                ActorConfig(path.r, rtype.r, ".*".r, 1, List(ActorMessage(x.clazz.r, x.message, 0, 0, 0, x.spath.r, ".*".r, ".*".r))))

        //println(s"PersistentActorRestart: \n\t$perturbable\n${config.map(x => "\t" + x).mkString("\n")}")

        config
    }
}
