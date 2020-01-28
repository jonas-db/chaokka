package be.vub.soft.debugging

import akka.actor.ActorSystem
import be.vub.soft.debugging.Status.Status
import org.scalatest.FlatSpec

class AutomatedDeltaDebuggingTest extends FlatSpec {

    "V1" should "work" in {
        val configuration = (1 to 8).toSet
        val test: Set[Int] => Status = i => if(i.contains(1)) Status.Fail else Status.Pass
        val expected: Set[Int] = Set(1)

        val result = AutomatedDeltaDebugging.dd[Int](configuration, test)

        assert(result == expected, s"Failed with $result")

        println(s"Result: $result")
    }

    "V2" should "work" in {
        val configuration = (1 to 80).toSet
        val expected: Set[Int] = Set(56)
        val test: Set[Int] => Status = i => if(i.intersect(expected) == expected) Status.Fail else Status.Pass
        val result = AutomatedDeltaDebugging.ddg[Int](configuration, test)

        assert(result._2 == expected, s"Failed with $result")

        println(s"Result: $result")
    }

    "V2" should "work with multiple faults" in {
        val configuration = (1 to 80).toSet // Set(1, 7, 3, 8, 4)
        val expected: Set[Int] = Set(1,67)
        //val test: Set[Int] => Status = i => if(i.intersect(expected) == expected) Status.Fail else Status.Pass
        val test: Set[Int] => Status = i => if(i.intersect(expected) == expected) Status.Fail else if(i.intersect(expected) == Set(1) || i.intersect(expected) == Set(67)) Status.Unresolved else Status.Pass
        //val test: Set[Int] => Status = i => if(i.intersect(expected) == expected) Status.Fail else Status.Pass
        val result = AutomatedDeltaDebugging.ddg[Int](configuration, test)
        val outcome = result._2.diff(result._1)

        assert(outcome == expected, s"Failed with $outcome")

        println(s"Result: $outcome")
    }

//    "Topology" should "work" in {
//        val test = List(
//            (1, List(5,4,3,2)),
//            (2, List(7,6)),
//            (3, List(7,6))
//        )
//        val alodFaults = List(2)
//        val persistenceFaults = List()
//
//        Topology.print(test, alodFaults, persistenceFaults)
//
//        val gen = Topology.generate(test, alodFaults, persistenceFaults)
//        val system = ActorSystem("Test")
//        val head = system.actorSelection(gen.head.path.name)
//        head ! Update(0, 1)
//
//
//
//    }

}
