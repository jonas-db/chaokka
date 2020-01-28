package be.vub.soft.topology

import be.vub.soft.tracer.{SendV2, TurnV2}

case class CausalityGraph(graph: Map[TurnV2, List[(TurnV2, SendV2)]] = Map.empty[TurnV2, List[(TurnV2, SendV2)]]) {

    val Color = "lightblue"
    val ColorNot = "gray"
    def format(s: String) = if(s.indexOf("/user/") > -1) s.substring(s.indexOf("/user/") + 6) else if(s.indexOf("/system/") > -1) s.substring(s.indexOf("/system/") + 8) else s

    def print(visited: Set[(TurnV2, SendV2)]) = {
        val mv = visited.map(_._1)
        val allNodes = graph.flatMap(g => g._1 :: g._2.map(_._1))
        val nodes = allNodes.toSet[TurnV2].map(x => {
            val color = if(mv.contains(x)) Color else ColorNot
            s""""${format(x.rpath)}, T${x.turnID}" [style=filled, fillcolor=$color];"""
        }).mkString("\n")

        println(nodes)

        graph.foreach({
            case (from, tuples) =>
                tuples.foreach({
                    case (to, s) => {
                        val color = if(visited.exists(v => v._2.equals(s))) Color else ColorNot
                        println(s""" "${format(from.rpath)}, T${from.turnID}" -> "${format(to.rpath)}, T${to.turnID}" [label="${s.clazz}, S${to.sendID}", color=$color, fontcolor=black]; """)
                    }
                })
        })
    }

    def collectPaths(root: TurnV2, to: String, maxTurnId: Int = -1): List[List[(TurnV2, SendV2)]] = {
        var set: List[List[(TurnV2, SendV2)]] = List(graph(root))
        var paths: List[List[(TurnV2, SendV2)]] = List()
        //println(set)

        while(set.nonEmpty) {
            val current: List[(TurnV2, SendV2)] = set.head
            set = set.tail

            val h = current.head

            // <=, probably never possible to be equal
            if(h._1.rpath.equals(to) && (maxTurnId == -1 || (maxTurnId != -1 && h._1.turnID <= maxTurnId))) { //
                //println(s"found path: $current")
                paths = current :: paths
                val ch = graph.getOrElse(h._1, List.empty)

                // optimize, stop adding if we have a turn > maxturnid
                ch.foreach(c => if(maxTurnId == -1 || (maxTurnId != -1 && c._1.turnID <= maxTurnId)) { //
                    set = (c :: current) :: set
                })
            } else if(graph.isDefinedAt(h._1)) {
                // extend path and push it
                //println(s"extending path: $current")
                val ch = graph(h._1)

                // optimize, stop adding if we have a turn > maxturnid
                ch.foreach(c => if(maxTurnId == -1 || (maxTurnId != -1 && c._1.turnID <= maxTurnId)) { //
                    set = (c :: current) :: set
                })
            } else {
                //println(s"path did not reach destination: $current")
            }
        }

        paths
    }


}
