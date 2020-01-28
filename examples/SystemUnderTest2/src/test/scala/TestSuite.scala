import akka.actor.{ActorSystem, Kill, Props, Terminated}
import akka.stable.Stabilizer
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, Matchers, OneInstancePerTest}

import scala.concurrent.duration._
import scala.concurrent.Await

class TestSuite() extends TestKit(ActorSystem("SystemUnderTest")) with FunSuiteLike with ImplicitSender with Matchers with BeforeAndAfterAll {

    test("p-1-1-1-1-X") {
        Stabilizer.init(5 * 1000)

        val a = system.actorOf(Props(new ActorA(this.testActor)),s"ActorA")
        val b = system.actorOf(Props(new ActorB(this.testActor)),s"ActorB")
        val c = system.actorOf(Props(new ActorC(this.testActor)),s"ActorC") // ActorC fails
        val d = system.actorOf(Props(new ActorD(this.testActor)),s"ActorD")
        val e = system.actorOf(Props(new ActorE(this.testActor)),s"ActorE")
        val x = system.actorOf(Props(new ActorX(this.testActor)),s"ActorX")

        a ! Update("-1", 1)

        Await.ready(Stabilizer.stabilize(), 5 * 60 seconds)

        val actors = List(a, b, c, d, e, x)
        val expected = List(1, 1, 1, 2, 3, 1)

        actors.foreach(_ ! State)

        var resultAll: List[(String, Int)] = List.empty[(String, Int)]

        while(resultAll.size != expected.size) {
            val r = receiveOne(Duration.Inf)
            r match {
                case m: (String, Int) => resultAll = m :: resultAll
                case m => m
            }
        }

        val sorted = resultAll.sortBy(_._1).zipWithIndex

        sorted.foreach(r => assert(r._1._2 == expected(r._2), s"Something failed, error@[${actors(r._2).path.toString}]"))
    }

    override def afterAll: Unit = {
        TestKit.shutdownActorSystem(system)
    }
}
