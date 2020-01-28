import akka.actor.{Actor, ActorRef}
import akka.persistence.{AtLeastOnceDelivery, PersistentActor}



class ActorA(testRef: ActorRef) extends PersistentActor with AtLeastOnceDelivery {

    var state: State = State()

    val faultyPersistence = false
    val faultyALOD = false
    var send = false

    def updateState(evt: Event): Any = evt match {
        case UpdateEvent(from, deliveryId) => {
            if(!state.messages.contains((from,deliveryId)) || faultyALOD) { // faulty == didn't implement idempotence of ALOD, TODO: persistence of i
                //println(s"Update state of $uuid: ${state.s} -> ${state.s+1} ($faulty)")
                send(from, deliveryId)
            }

            // ALWAYS CONFIRM HERE! if we confirm in send, it might never be confirmed!
            //val p = if(from == -1) context.parent.path.parent./(List("system", s"testActor-1")) else context.system.child(s"actor-$from")
            val p = if(from == "-1") testRef.path else context.system.child(s"$from")
            val pp = context.actorSelection(p)
            pp ! Confirm(persistenceId, deliveryId)

            //println(p)
            //println(s"Confirming $from $deliveryId")
        }
        case ConfirmEvent(from, deliveryId) => {
            val t = (from, deliveryId)
            //state = state.copy(messages = (state.messages - t))
            confirmDelivery(deliveryId)

            val next = List("ActorB", "ActorX")

            if(from.contains("ActorE") && !send) {
                send = true
                next.foreach(c => {
                    val p = context.system.child(c)
                    deliver(p)(deliveryId => {
                        //println(s"${context.self.path.name} -> actor-$c ($deliveryId)")
                        Update(persistenceId, deliveryId)
                    })
                })
            }

        }

        //case unhandled => assert(assertion = false, s"Unhandled event: $unhandled")
    }

    override def receiveRecover: Receive = {
        case evt: Event => updateState(evt)
    }


    def send(from: String, id: Long) = {
        val t = (from, id)


        // Update state, but if faultyPersistence == true, we "forget to update as the state change was not captured"
        // We basically mimic a forgotten persist

        // here we simulate that the state is not recovered, 'we forget to update state == we did not persist state'
        val newState = if(faultyPersistence && !recoveryFinished) state.s else (state.s + 1)
        state = state.copy(s = newState, messages = (state.messages + t))

        val children = List( "ActorE")

        if(!from.contains("ActorE")) {
            // Do side effects
            children.foreach(c => {
                val p = context.system.child(c)
                deliver(p)(deliveryId => {
                    //println(s"${context.self.path.name} -> actor-$c ($deliveryId)")
                    Update(persistenceId, deliveryId)
                })
            })
        }



    }

    override def receiveCommand: Receive = {
        case State => {
            //println(state)
            sender() ! (persistenceId, state.s)
        }
        case Update(from, id) => {
            //println(s"${context.self.path.name} received update (state=$state)")
            persist(UpdateEvent(from, id))(updateState) // We only persist that we got the message
        }
        case Confirm(from, deliveryId) => persist(ConfirmEvent(from, deliveryId))(updateState)
        //case "shutdown" => context.stop(self)
        // Restart command to reset state to initial => avoid restartingg actor system
        //case unhandled => assert(assertion = false, s"Unhandled command: $unhandled")
    }

    override def persistenceId: String = this.getClass.getName
}
