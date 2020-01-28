trait Event

case object State
case class Update(from: String, id: Long)
case class Confirm(from: String, id: Long)
case class UpdateEvent(i: String, id: Long) extends Event
case class StateEvent(i: Int, id: Long) extends Event
case class ConfirmEvent(from: String, id: Long) extends Event

case class State(s: Int = 0, messages: Set[(String, Long)] = Set.empty)