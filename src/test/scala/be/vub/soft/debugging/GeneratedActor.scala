package be.vub.soft.debugging

import java.io.PrintWriter

import akka.actor.{Actor, ActorSystem, Props}
import akka.persistence.{AtLeastOnceDelivery, PersistentActor}

trait Event
case object State
case class Update(i: Int, id: Long)
case class Confirm(id: Long)
case class UpdateEvent(i: Int, id: Long) extends Event
case class ConfirmEvent( id: Long) extends Event

case class State(s: Int = 0, messages: Set[Long] = Set.empty)

/*
    even if all actors are asserted, one assertion will fail but you don't know which duplicated message caused it -> narrow it down to the correct one (probably is the one though)
    one duplicated message probably fails multiple assertions as they are propagated through the system

    TODO: faultyreply

    ALOD:
        it should persist state multiple times, and forward persisted state, check idempotence

    Restart:
        it should forward the unpersisted state (= 0)

    assumptions:
        message uniqueness -- every message is uniquely identifiable
        observability -- a resilience issues should be observable through assertions
            if they didn't assert all, they won't get a minimal set. same applies to assert only a field of an object instead of all
        propagate error -- see observability
        non-determinism -- in actors an messages, ordering should not matter, as perturbations are applied when their condition appears (order of trace)
        functionality is correct (test succeeds without neutral events) -- but not sure if it can work with partial failures
        eventual consistent -- a call to the system will eventually cause the expected value, retries can happen, a message will eventually arrive
 */
class GeneratedActor(uuid: Int, faulty: Boolean, faultyPersistence: Boolean, children: List[Int] = List.empty, var state: State = State()) extends PersistentActor with AtLeastOnceDelivery {

    override def receiveCommand: Receive = {
        case State => sender() ! state
        case Update(i, id) => if(!faultyPersistence) {
            persist(UpdateEvent(i, id))(updateState)
        } else {
            send(id) // do not persist an event, send directly
        }
        case Confirm(deliveryId) => persist(ConfirmEvent(deliveryId))(updateState)
        // Restart command to reset state to initial => avoid restartingg actor system
        //case unhandled => assert(assertion = false, s"Unhandled command: $unhandled")
    }

    def send(id: Long) = {
        state = state.copy(s = (state.s + 1), messages = (state.messages + id))
        children.foreach(c => {
            val p = context.system.actorSelection(s"actor-$c")
            deliver(p)(id => Set(state.s, id))
        })
        sender() ! Confirm(id)
    }

    def updateState(evt: Event): Any = evt match {
        case UpdateEvent(i, id) => {
            if(!state.messages.contains(id) || faulty) { // faulty == didn't implement idempotence of ALOD, TODO: persistence of i
                send(id)
            }
        }
        case ConfirmEvent(deliveryId) => {
            state = state.copy(messages = (state.messages - deliveryId))
            confirmDelivery(deliveryId)
        }

        //case unhandled => assert(assertion = false, s"Unhandled event: $unhandled")
    }

    override def receiveRecover: Receive = {
        case evt: Event => updateState(evt)
    }

    override def persistenceId: String = s"exercise-$uuid"

}


object Topology {
    def print(test: List[(Int, List[Int])], alodFaults: List[Int], persistenceFaults: List[Int]) = {
        val edges = test.flatMap({case (a, children) => children.map(c => {
            val color = if(alodFaults.contains(c)) "red" else "black"
            val label = if(alodFaults.contains(c)) "duplicate" else "\"\""
            val fontcolor = if(alodFaults.contains(c)) "red" else "black"
            s"${'"'}$a${'"'} -> ${'"'}$c${'"'} [color=$color, label=$label, fontcolor=$fontcolor];"
        })}).mkString("\n")

        val nodes = test.map({case (a, _) => {
            val color = if(alodFaults.contains(a)) "red" else "black"
            s"$a [color=$color];"
        }}).mkString("\n")

        println(s"digraph G { $nodes \n $edges }")

        new PrintWriter("./topology.dot") {write(s"digraph G { $nodes \n $edges }"); close}
        import sys.process._
        "dot -Tpdf topology.dot -o topology.pdf" !!
    } //


    val system = ActorSystem("pingpong")

    def generate(topology: List[(Int, List[Int])], alodFaults: List[Int], persistenceFaults: List[Int]) = topology.map({
        case (uuid, children) =>
            system.actorOf(Props(new GeneratedActor(uuid, alodFaults.contains(uuid), persistenceFaults.contains(uuid), children)), "actor-"+uuid)
    })

}
