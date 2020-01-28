package be.vub.soft.perturbations

import be.vub.soft.Main
import be.vub.soft.parser.{ActorConfig, ActorMessage}
import be.vub.soft.tracer.{ActorRegistration, Send, TestReport, Traceable}
import be.vub.soft.utils.Constants

/*
    Inject an actor restart (e.g. due to a node going down)
 */

//object ActorRestart extends Perturbation {
//
//    override def pre(traceable: Traceable, report: TestReport): Boolean = traceable match {
//        case a: ActorRegistration if !a.inheritance.contains(Constants.PersistentActor) && !a.inheritance.contains(Constants.AtLeastOnceDelivery) => true
//        case _ => false
//    }
//
//    override def inject[ActorRegistration](perturbable: ActorRegistration, report: TestReport, messageCandidates: Set[Traceable], actorCandidates: Set[Traceable]): List[ActorConfig] = perturbable match {
//        case ActorRegistration(_, _, _, path, _, _, rtype, _, _) =>
//
//        //val config = ActorConfig(rpath.r, rtype.r, ".*".r, Main.Probability, List.empty)
//
//        val config = messageCandidates.asInstanceOf[Set[Send]].toList
//            .filter(x => !x.clazz.startsWith("akka") && x.rpath.equals(path))
//            .map(x => ActorConfig(path.r, rtype.r, ".*".r, Main.Probability, List(ActorMessage(x.clazz.r, x.message, 0, 0, 0, x.spath.r, ".*".r, ".*".r))))
//
//        println(s"ActorRestart: \n\t$perturbable\n${config.map(x => "\t" + x).mkString("\n")}")
//
//        config
//    }
//}
