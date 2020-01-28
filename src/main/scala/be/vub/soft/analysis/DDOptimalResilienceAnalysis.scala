package be.vub.soft.analysis

import be.vub.soft.parser.{ActorConfig, ActorMessage}
import be.vub.soft.perturbations.Perturbation
import be.vub.soft.topology.{CausalityGraph, Topology}
import be.vub.soft.tracer.{ActorRegistration, ActorTurn, SendV2, TestReport, TurnV2}
import be.vub.soft.utils.Utils

class DDOptimalResilienceAnalysis(perturbation: Perturbation) extends DDResilienceAnalysis(perturbation) {

    override def name = "DDOptimal"

    var affected:Option[Set[SendV2]] = None

    val debugOptimal = false
    def log(s: String) = if(debugOptimal) println(s) else ()

    def getErroneousActor() = {
        val regex = """error@\[.*?\]""".r
        this.exception.flatMap(ex => regex.findFirstIn(ex).map(s => s.substring(7, s.length - 1)))
    }

    override def toSubset(diff: Set[ActorConfig], n: Int): List[Set[ActorConfig]] = {
        // calculate all paths from 0 the failing assertion, group all perturbations on these paths in one subset
        val error = getErroneousActor()

        // calculate affected actors once there is an error
        // affected perturbations are only computed once, as it is based on the initial trace only
        if(error.nonEmpty && affected.isEmpty) {
            affected = Some(affectedMessages(this.initialReport.get, error.get))

            //println(s"affected: ${affected.size}")
            //affected.foreach(println(_))
        }

        if(affected.nonEmpty) {
            val onPath: Set[ActorConfig] = diff.collect({
                // collect only the perturbations that are affected
                case a@ActorConfig(actorName, _, _, _, List(ActorMessage(_, hash, _, _, _, senderName, _, _)))
                    if affected.get.exists(af => af.message.equals(hash) && af.spath.equals(senderName.toString()) && af.rpath.equals(actorName.toString())) => a
            })

            //println(s"onpath: ${onPath.size}")
            //onPath.foreach(println(_))

            //TODO: we can still apply heuristics here to group certain subsets together, ie most important actors first
            val list:List[ActorConfig] = onPath.toList
            val minGroup: Int = onPath.size / n
            val minFix = if(minGroup < 1) 1 else minGroup
            list.grouped(minFix).toList.map(x => x.toSet)
        } else {
            val list:List[ActorConfig] = diff.toList
            list.grouped(diff.size / n).toList.map(x => x.toSet)
        }
    }

    def affectedMessages(report: TestReport, errorActor: String) = {
        log(s"Search for paths to $errorActor")

        // All user messages
        val sends = report.trace.collect({ case s: SendV2 => s })
        val turns = report.trace.collect({ case s: TurnV2 => s })

        val filteredTurns = turns.filter(s => !s.clazz.contains("akka"))
        val filteredSends = sends.filter(s => !s.clazz.contains("akka"))
        //filteredTurns.foreach(println(_))
        //println("----")
        //filteredSends.foreach(println(_))

        var graph: Map[TurnV2, List[(TurnV2, SendV2)]] = Map.empty[TurnV2, List[(TurnV2, SendV2)]]
        val Root = TurnV2("System", -1, "TestActor", -1, 0, "Unknown", -1, -1)
        val RootSend = SendV2("System", -1, "TestActor", -1, 0, "Unknown", -1, -1)

        turns.foreach(t => {
            //println(t)

            // find send that caused this turn
            val s = sends.find(s => s.sendID == t.sendID)
            //println(s)

            // find (previous) turn that caused the message
            var o = turns.find(t => t.turnID == s.get.turnID).getOrElse(Root)
            //println(o)

            val cls = s.get.clazz

            // Akka messages from user to user actor
            if(!cls.contains("WriteMessageSuccess") && !cls.contains("LoadSnapshotResult") && !cls.contains("WriteMessagesSuccessful") && !cls.contains("RecoverySuccess") && !cls.contains("RecoveryPermitGranted")) {
                val current: List[(TurnV2, SendV2)] = graph.getOrElse(o, List.empty)
                val nw: List[(TurnV2, SendV2)] = (t, s.get) :: current
                graph = graph + (o -> nw)
            }
        })

        // !!! we remove all interaction with (from/to) the test actor, because otherwise we could not safe much, thus we assume test actors do not affect the system (ie only querying) !!!
        val fixedGraph = graph.filter({ case (k,v) => !k.rpath.contains("testActor")}).map({ case (k,v) => (k -> v.filter(x => !x._2.rpath.contains("testActor")))}).filter(m => m._2.nonEmpty) // if there are nodes in the map, with no children, we know these were related to the test actor because normally, turns without children are NOT in the map
        val totalTurns = fixedGraph.flatMap({ case (k,v) => k :: v.map(_._1)}).toSet[TurnV2].size

        val cg = CausalityGraph(fixedGraph)

        // Step 1. Get all paths to error actor E, put turns in a set, A is the set of all actors of which a turn is in the set
        val paths: List[List[(TurnV2, SendV2)]] = cg.collectPaths(Root, errorActor)
        val turnsOfPaths = paths.flatten.toSet

        log("------------------- paths ------------------")

        log(paths.map(l => l.reverse.mkString("\n")).mkString("\n --- \n"))

        log("------------------- algo ------------------")

        // Visited becomes all turns we need
        var visited: Set[(TurnV2, SendV2)] = Set((Root, RootSend))
        var todo: List[(TurnV2, SendV2)] = turnsOfPaths.toList // TODO: toset?

        while(todo.nonEmpty) {
            val current = todo.head
            todo = todo.tail

            // add to visited
            visited = visited + current

            // Step 2. For each turn of an actor a@A, find all paths that affected
            val extraPaths = cg.collectPaths(Root, current._1.rpath, current._1.turnID)
            val extraActors = extraPaths.flatten.toSet

            // extract only the ones that we didn't visit yet, and add them to todo list
            val diff = extraActors.diff(visited)
            todo = todo ++ diff
        }

        log(s"------------------- visited (${visited.size}/${totalTurns})------------------")

        visited.foreach(x => log(x.toString))
        visited.foreach(p => log(s"${p._2.clazz},${p._2.sendID}"))

        log("------------------------------------------------")

        //cg.print(visited)

        visited.map(_._2)
    }
}
