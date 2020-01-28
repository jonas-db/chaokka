package be.vub.soft.utils

import java.io.{File, PrintWriter}
import java.nio.file.Paths
import sys.process._
import be.vub.soft.tracer.{ActorRegistration, ActorTurn, Traceable}

object Utils {
    def getID(spath: String) = spath.substring(spath.indexOf("actor-") + 6).toInt


    def getGraph(trace: List[Traceable]) = {
        var actors = Set.empty[String]
        var messages = Set.empty[(String, String, String)]

        trace.foreach({
            case a: ActorTurn if(a.spath.contains("/user/") && a.rpath.contains("/user/") && !a.clazz.contains("akka.")) => {
                val t = (a.spath, a.rpath, a.clazz)
                messages = messages + t
            }
            case c: ActorRegistration => if(c.childPath.contains("/user/")) {
                actors = actors + c.childPath
            }
            case _ => ()
        })

        (actors, messages)
    }

    def printGraph(outFolder: File, trace: List[Traceable], n: Int) = {
        var graph = List.empty[String] // [fillcolor=red, style=filled]
        def add(p: String, c: String) = graph = graph :+ s"${'"'}$p${'"'} -> ${'"'}$c${'"'}"
        var messages = Map.empty[(String, String, String), Int]
        def addMessage(s: String, r: String, c: String) = graph = graph :+ s"${'"'}$s${'"'} -> ${'"'}$r${'"'} [color=${'"'}0.002 0.999 0.999${'"'}, label=${'"'}$c${'"'}];"
        val all = false
        var actors = 0
        var setOfActorTypes = Set.empty[String]
        trace.foreach({
            //                case s: Send => if(all || s.spath.contains("/user/") && s.rpath.contains("/user/") && !s.clazz.contains("akka.")) { //
            //                    val k = (s.spath, s.rpath, s.clazz)
            //                    val v = messages.getOrElse(k, 0) + 1
            //                    messages = messages + (k -> v)
            //                }
            case a: ActorTurn if(a.spath.contains("/user/") && a.rpath.contains("/user/") && !a.clazz.contains("akka.")) => {
                val k = (a.spath, a.rpath, a.clazz)
                val v = messages.getOrElse(k, 0) + 1
                messages = messages + (k -> v)
            }
            case c: ActorRegistration => if(all || c.childPath.contains("/user/")) {
                add(c.parentPath, c.childPath)
                actors = actors + 1
                setOfActorTypes = setOfActorTypes + c.actorType
            }
            case _ => ()
        })
        messages.foreach({ case (k,v) => addMessage(k._1, k._2, s"${k._3} ($v)")})


        val gg = graph.mkString("\n")
        val inFile = Paths.get(outFolder.getAbsolutePath, s"graph-$n.dot").toString
        val outFile = Paths.get(outFolder.getAbsolutePath, s"graph-$n.pdf").toString
        new PrintWriter(inFile) { write(s"digraph G { $gg }"); close() }
        s"dot -Tpdf $inFile -o $outFile" !!
    }

}
