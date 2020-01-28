package be.vub.soft.sut

import akka.actor.{ActorSystem, Kill, Terminated}
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, InMemorySnapshotStorage, StorageExtension}
import akka.stable.Stabilizer
import akka.testkit.{ImplicitSender, TestKit}
import be.vub.soft.topology.{Confirm, State, Topology, TopologyO, Update}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, Matchers, OneInstancePerTest}

import scala.concurrent.duration._
import scala.concurrent.Await

/*
    The test outcomes will fail because they are put in a single actor system, this was done to make the evaluation easier.
    All tests work when ran individually.
 */

class TestSuite() extends TestKit(ActorSystem("SystemUnderTest")) with FunSuiteLike with ImplicitSender with Matchers with BeforeAndAfterAll {

    val NODES = 50
    val faults50 = List(5, 25, 45)

    val topologies = List(
        // Target Perturbations, Perturbations
        // 100, 129
        (List((0,List(21, 1, 16, 4, 13, 12, 23, 30, 48)), (5,List(6, 8, 26, 46, 20, 31)), (10,List(34)), (24,List(30)), (25,List(30, 29)), (14,List(29, 46, 41, 24)), (20,List(34)), (29,List(30)), (1,List(2, 15, 26, 23, 40, 32, 20, 10)), (6,List(39, 20, 48, 44, 49)), (28,List(33, 49)), (21,List(28, 39, 37, 30, 46)), (9,List(11, 42)), (41,List(44)), (2,List(5, 38, 11, 3)), (32,List(49, 36)), (34,List(47)), (17,List(32, 34, 35, 25, 49)), (22,List(44, 46)), (44,List(47, 45)), (27,List(29)), (12,List(44, 46, 42)), (7,List(19, 34, 14)), (39,List(42)), (3,List(30, 26, 36)), (35,List(46, 45)), (18,List(26)), (16,List(42)), (31,List(43, 38, 34)), (11,List(18, 36, 46)), (43,List(45)), (23,List(34)), (8,List(22, 18, 48)), (19,List(23, 45, 24)), (4,List(9, 7, 13, 17)), (15,List(27, 49))), 0),
        // 200, 204
        (List((0,List(6, 1, 15, 18, 19, 2, 42, 32)), (5,List(14, 11, 27, 48)), (10,List(25, 23, 44)), (42,List(43, 47)), (24,List(27, 38, 34)), (25,List(29, 46, 47)), (20,List(44, 37, 34)), (46,List(47)), (1,List(13, 43, 45, 28)), (6,List(23, 20, 44, 48, 25, 32)), (28,List(29, 39)), (38,List(43)), (21,List(46, 30)), (33,List(36)), (9,List(47, 16, 22, 25, 12)), (13,List(36)), (2,List(3, 17, 10, 7)), (32,List(47, 45)), (34,List(38)), (45,List(47)), (17,List(31, 30, 33)), (22,List(49, 44, 33)), (27,List(31, 41)), (12,List(44, 43, 23, 29)), (7,List(39, 26, 48)), (3,List(8, 21, 5, 4, 40, 48, 10)), (35,List(46, 40)), (18,List(27, 20, 33, 43)), (11,List(34, 38)), (43,List(44)), (40,List(44, 49)), (23,List(49, 26)), (8,List(33)), (36,List(42)), (30,List(35, 33)), (19,List(46, 45, 44, 39)), (4,List(9, 47, 39, 41)), (15,List(24, 27, 46))),0),
        // 300, 3O8
        (List((0,List(1, 12, 21, 15)), (5,List(6, 40, 37)), (10,List(14, 21, 38)), (42,List(45)), (24,List(45)), (25,List(34, 49)), (14,List(28, 20, 23, 46)), (20,List(29, 37, 45, 23, 21, 30)), (46,List(48)), (29,List(40, 37)), (1,List(2, 3, 4, 29, 15)), (6,List(29, 20, 45, 22)), (28,List(48)), (38,List(40)), (21,List(23, 44, 40, 46, 26)), (33,List(44)), (9,List(41, 24, 38, 43, 15)), (13,List(33, 37, 30, 17, 39, 25)), (2,List(7, 9, 38, 20)), (32,List(48, 37)), (34,List(46)), (17,List(44, 47)), (22,List(28, 23, 44)), (27,List(31, 32)), (12,List(16, 28, 36, 45, 39)), (7,List(15, 22)), (39,List(41, 45)), (3,List(5, 11, 10, 20, 48)), (35,List(45)), (48,List(49)), (18,List(43)), (16,List(27, 18)), (31,List(36)), (11,List(13, 20)), (43,List(49)), (26,List(37, 43)), (23,List(47, 42)), (8,List(28, 44, 38)), (19,List(35, 46, 22)), (4,List(8, 25, 19)), (47,List(49)), (15,List(31))), 0),
        // 400, 378
        (List((0,List(1, 15, 2, 35, 6)), (5,List(9, 40, 35, 14)), (10,List(22, 37, 45, 39)), (24,List(31, 26, 46)), (25,List(32, 36, 39)), (14,List(30, 31)), (20,List(36)), (46,List(47)), (29,List(40, 41)), (1,List(7, 17, 3, 5, 18, 4, 25)), (6,List(35)), (28,List(33, 46)), (21,List(31)), (33,List(47)), (9,List(19, 29, 27, 14, 43, 31)), (13,List(19, 46)), (2,List(10, 41, 26, 27)), (32,List(48)), (34,List(40, 41)), (17,List(23)), (22,List(30, 35)), (44,List(45)), (27,List(43, 34)), (12,List(19)), (7,List(11, 8, 42, 30, 33)), (39,List(41)), (3,List(16, 44, 20, 35)), (35,List(44)), (18,List(44, 28, 36)), (16,List(42)), (31,List(37)), (11,List(30, 12)), (43,List(44)), (26,List(39, 37, 36)), (23,List(48, 39)), (8,List(36, 26, 24, 49, 37, 32, 14)), (30,List(37, 43, 33)), (19,List(33)), (4,List(13, 24)), (15,List(38, 21, 40))),1),
        // 500, 513
        (List((0,List(1, 4, 2, 13, 43, 22, 44, 47, 3, 32, 46)), (5,List(11, 47, 23, 10, 28, 12)), (10,List(18, 21, 19, 15)), (42,List(49)), (24,List(29)), (37,List(38)), (25,List(27)), (14,List(29, 33)), (20,List(43, 44, 40)), (29,List(49, 30, 47, 41, 40, 45)), (1,List(16, 48, 28, 24)), (28,List(33, 45)), (38,List(47)), (21,List(28, 45, 31, 23)), (33,List(34)), (9,List(32)), (13,List(23, 17)), (2,List(8, 5, 15, 3)), (32,List(36)), (34,List(36, 42)), (17,List(45)), (22,List(23, 37, 35, 39)), (44,List(47)), (27,List(45, 36, 47)), (12,List(28, 24, 13, 44)), (7,List(35, 18, 20)), (39,List(47, 40, 41)), (3,List(20, 43, 34, 27, 9, 14)), (35,List(42, 47)), (18,List(42, 37)), (16,List(47, 43, 38, 26, 49, 41, 37)), (31,List(38, 45, 47, 39, 48)), (11,List(29, 35, 31, 47, 41)), (40,List(42)), (26,List(33, 35, 42, 46)), (23,List(32)), (8,List(45, 23, 29, 25)), (36,List(45, 41)), (30,List(37, 43, 38)), (19,List(27)), (4,List(6, 7, 5, 42, 43)), (15,List(17, 38, 47))), 0),
        // 600, 626
        (List((0,List(1, 5, 10, 36, 35, 44, 29, 2)), (5,List(21, 9)), (10,List(14, 49, 46, 25, 16)), (37,List(45)), (25,List(29, 42)), (14,List(28, 44, 35)), (20,List(34, 41, 36)), (46,List(48)), (29,List(32, 30)), (1,List(12, 8, 49, 41, 18)), (6,List(7, 18, 39, 37)), (28,List(36, 33, 31, 48)), (38,List(46)), (21,List(24, 31, 42, 43, 49, 33)), (33,List(38, 36)), (9,List(11, 15, 38)), (13,List(45)), (2,List(3, 4, 6, 49)), (32,List(33)), (45,List(46)), (17,List(27, 28, 29, 49)), (22,List(45, 39, 41)), (44,List(46)), (27,List(48)), (12,List(29, 33, 23, 47, 34)), (7,List(17, 22, 39, 31, 34, 26)), (39,List(48, 40)), (3,List(19, 38)), (18,List(48, 23, 44, 25, 26, 33)), (16,List(32, 49, 33)), (11,List(13, 40)), (26,List(34, 31)), (23,List(48)), (8,List(39, 48)), (36,List(41)), (30,List(32, 44)), (19,List(46, 20, 33)), (4,List(45, 41, 13, 40, 36)), (47,List(48)), (15,List(26, 24))),1),
        // 700, 706
        (List((0,List(1, 10, 27, 15)), (5,List(8, 26, 7)), (10,List(23, 31)), (24,List(47, 33, 46)), (37,List(43)), (25,List(44, 40, 43, 32)), (14,List(20, 18)), (20,List(25, 35, 23, 49)), (29,List(39)), (1,List(2, 33, 37, 13, 24)), (6,List(17, 15, 18)), (28,List(39, 36, 32)), (38,List(42)), (21,List(31)), (33,List(42)), (9,List(13, 44, 17, 43, 24, 28, 18, 31)), (13,List(39, 28, 33, 34, 24, 48)), (41,List(43)), (2,List(3, 5, 41, 30, 34)), (17,List(36, 30, 28)), (22,List(23)), (27,List(36, 47, 41)), (12,List(26, 44, 38, 46, 40)), (7,List(9, 25, 14, 29, 46, 22, 18, 11)), (39,List(44, 49)), (3,List(6, 13, 4, 14)), (18,List(21, 24, 23)), (31,List(37, 47, 36, 46)), (11,List(12, 44)), (26,List(32)), (23,List(45)), (8,List(26, 16, 44, 17)), (36,List(38)), (30,List(34)), (19,List(44, 35)), (4,List(40, 27, 17, 29)), (15,List(19, 39, 35, 41, 45))),1),
        // 800, 854
        (List((0,List(1, 8, 2, 36, 29, 10, 17, 27, 35)), (5,List(22, 44)), (10,List(12, 47, 28, 23, 38, 49, 39)), (24,List(39, 35)), (37,List(42)), (25,List(43, 28, 40, 42)), (14,List(48, 17, 32)), (20,List(35, 38, 41)), (29,List(40, 44, 46)), (1,List(14, 13, 7)), (6,List(21, 30, 46, 17)), (28,List(32, 48)), (38,List(44, 49)), (21,List(33, 29)), (33,List(46, 35, 41)), (9,List(21, 33, 13, 32, 48)), (13,List(15, 34, 42)), (2,List(3, 4, 6, 46, 49, 22, 20, 19, 26, 5)), (32,List(43)), (34,List(39, 37)), (17,List(47, 34, 22, 41, 37)), (22,List(31, 46, 43, 47)), (44,List(45, 47)), (27,List(35)), (12,List(16, 28, 41, 31)), (7,List(29, 31, 15, 9, 18)), (39,List(41, 48)), (3,List(9, 40, 25, 36, 18)), (35,List(48, 39, 43)), (16,List(47, 43, 36, 26)), (11,List(24, 27, 25, 46, 30, 23, 36)), (40,List(44)), (26,List(35, 45)), (23,List(35, 44, 32, 31)), (8,List(19, 11)), (36,List(48)), (19,List(48)), (4,List(17, 32, 39, 9)), (15,List(30))),1),
        // 900, 885
        (List((0,List(1, 4, 28, 34)), (5,List(7, 46, 21)), (10,List(41, 33, 37, 19, 11, 36)), (24,List(32)), (25,List(26, 48, 36, 33, 27, 34)), (14,List(37, 25, 35, 32, 31, 36)), (20,List(21, 30, 44)), (29,List(33, 49)), (1,List(2, 3, 18, 40, 37, 16, 15, 21, 6, 35, 46)), (6,List(31, 32, 12, 41)), (28,List(29, 33, 30)), (21,List(25, 22, 45)), (33,List(43, 37, 34)), (9,List(45)), (13,List(25, 43, 44, 35)), (41,List(45, 42)), (2,List(46, 21, 45, 48, 19, 27, 5)), (32,List(43, 40)), (34,List(42)), (17,List(26, 46)), (22,List(38, 37, 46)), (44,List(49)), (27,List(32, 45, 49)), (12,List(48, 19, 28, 20, 32)), (7,List(19, 26, 30, 29, 35, 22, 38)), (39,List(49, 48)), (3,List(17, 20, 8, 45, 48, 9, 38, 14)), (35,List(48, 45)), (18,List(28, 27)), (16,List(38, 18, 36)), (31,List(36, 44, 33)), (11,List(13, 46, 15)), (40,List(47, 44, 48)), (26,List(49, 39, 31, 34)), (23,List(36)), (8,List(42, 17, 9, 49, 20, 11)), (30,List(46)), (4,List(42, 10)), (47,List(49)), (15,List(23, 42, 43, 24, 31, 21, 33))),0),
        // 1000, 1005
        (List((0,List(1, 15, 4, 32, 24, 20, 23, 13, 2)), (5,List(6, 39, 18, 10, 22)), (10,List(21, 48, 17, 38)), (24,List(33, 38, 44, 46)), (37,List(38)), (14,List(41, 30, 45, 21, 38)), (20,List(26)), (29,List(43, 47)), (1,List(42, 2, 3, 39, 14, 43, 25, 29, 12, 38, 21)), (6,List(35, 10)), (28,List(33, 45, 31, 32)), (38,List(40, 49, 44)), (21,List(28, 34)), (33,List(37, 44)), (9,List(42, 33, 29, 49, 45, 20)), (13,List(44, 24)), (41,List(48, 46)), (2,List(38, 45, 33, 13, 37, 9)), (32,List(43, 44)), (17,List(25, 48, 18, 36)), (22,List(45, 25, 34, 40, 49, 38)), (44,List(49)), (12,List(35, 14, 28)), (7,List(39, 20, 38, 37, 49)), (39,List(44)), (3,List(38, 35, 49, 43, 12, 47, 34, 10)), (16,List(29, 18, 36, 48, 49)), (11,List(19, 47, 32, 26)), (43,List(48)), (40,List(43, 48, 44)), (26,List(36, 28, 27)), (23,List(35, 33, 40)), (8,List(11, 29, 16, 9)), (30,List(34, 39)), (19,List(30, 42, 26)), (4,List(5, 7, 20, 8, 26, 23)), (15,List(48, 17, 40, 45))),0)
    )

    val permutations50: List[(Int, Int, List[(Int, List[Int])], Int, Int)] = (for {
        (i1, k) <- topologies
        i2 <- faults50
    } yield (NODES, k, i1, i2)).zipWithIndex.map(tpl => (tpl._1._1,tpl._1._2,tpl._1._3,tpl._1._4,tpl._2))

    permutations50.foreach(l => {
        registerResilienceTest(l)
    })

    var overhead = List(
        // 1000, 1058
        (List((0,List(1, 4, 2, 45, 11, 21, 49, 24, 29, 22)), (5,List(32, 6, 26, 7, 19, 44)), (10,List(47, 13, 39, 34, 16, 49)), (24,List(47)), (25,List(45)), (14,List(49, 26, 37, 34, 39, 45, 44)), (20,List(41, 48, 38, 35, 32)), (46,List(47)), (29,List(44, 48, 35)), (1,List(3, 23, 10, 22, 37, 45, 29, 2, 25)), (6,List(47, 12, 46, 17, 28)), (28,List(42, 38, 41)), (38,List(47)), (21,List(41, 30, 25, 44, 26)), (33,List(43)), (9,List(15, 25, 49, 21, 17, 20, 30, 38, 36, 45)), (13,List(29, 34, 27, 43, 46, 14, 32)), (41,List(49, 42)), (2,List(15, 12, 46)), (32,List(41, 40, 35)), (34,List(39, 43)), (17,List(26, 43, 18, 22, 42, 49)), (22,List(42, 37)), (27,List(39, 48, 28, 46)), (12,List(40, 25, 16, 21, 49)), (7,List(49, 13, 32, 31, 28, 9, 34, 26)), (3,List(8, 36, 7, 28, 40, 9)), (35,List(37, 40, 44, 48, 41, 42)), (18,List(24, 40, 26, 33, 43)), (16,List(31, 46)), (31,List(35, 32, 46, 44)), (11,List(29, 32, 15)), (43,List(49)), (40,List(47)), (26,List(30, 38, 41, 42)), (23,List(36, 39, 33, 37)), (8,List(45, 38, 35, 42, 47)), (30,List(46, 33, 40)), (19,List(21, 34, 26, 25, 30, 47, 49, 42, 43)), (4,List(5, 45, 10, 28, 44)), (15,List(43, 16, 26, 30, 42, 27))),0),
    )

    def registerResilienceTest(l: (Int, Int, List[(Int, List[Int])], Int, Int)) = {
        val nodes       = l._1
        val tvar        = l._2
        val t           = l._3
        val fault       = l._4
        val idx         = l._5

        // topology variant decides the fault
        if(tvar == 0) {
            val t1 = s"a-$nodes-$idx-0-$tvar-$fault"
            test(t1) {
                assertStableSystem(t1, t, Set(fault))
            }
        } else {
            val t2 = s"p-$nodes-$idx-0-$tvar-$fault"
            test(t2) {
                assertStableSystem(t2, t, Set(), Set(fault))
            }
        }
    }

    def assertStableSystem(name: String, t: List[(Int, List[Int])], alodFaults: Set[Int] = Set(), persistenceFaults: Set[Int] = Set()): Unit = {
        Stabilizer.init(5 * 1000)

        val topology = Topology(t, alodFaults, persistenceFaults)
        //topology.print(name)

        val refs = TopologyO.generate(system, topology, testActor)
        val mapped: Map[String, String] = refs.map(x => (x.path.name -> x.path.toString)).toMap
        val expected = refs.map(r => (r.path.toString -> topology.findPaths(r.path.name.substring(6).toInt))).toMap

        val head = system.actorSelection(system.child(s"actor-0"))
        head ! Update(-1, 1)

        val time = Await.ready(Stabilizer.stabilize(), 60 * 60 seconds)

        refs.foreach(_ ! State)

        var resultAll: List[(Int, Int)] = List.empty[(Int, Int)]

        while(resultAll.size != expected.size) {
            val r = receiveOne(Duration.Inf)
            r match {
                case m: (Int, Int) => resultAll = m :: resultAll
                case m => m
            }
        }

        val sorted = resultAll.sortBy(_._1).zipWithIndex

        sorted.foreach(r => {
            val p = mapped("actor-"+r._2)
            assert(r._1._2 == expected(p), s"Something failed, error@[${p}]")
        })

    }

    override def afterAll: Unit = {
        TestKit.shutdownActorSystem(system, 5 * 60 seconds)
    }
}
