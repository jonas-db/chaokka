package be.vub.soft.debugging

import be.vub.soft.analysis.ResilienceAnalysis
import be.vub.soft.debugging.Status.Status
import be.vub.soft.parser.ActorConfig

import scala.tools.nsc.typechecker.Analyzer

object Status extends Enumeration {
    type Status = Value
    val Pass, Fail, Unresolved = Value
}

sealed abstract class DDOutcome
case class DDResult(cok: Set[ActorConfig], cnok: Set[ActorConfig]) extends DDOutcome
case object DDTimeout extends DDOutcome
case object DDNoErrorFound extends DDOutcome

object AutomatedDeltaDebugging {

    def debug(s: String) = if(false) println(s)
    val DefaultSubsets: (Set[ActorConfig], Int) => List[Set[ActorConfig]] = (diff, n) => diff.grouped(diff.size / n).toList

    def dda(cnok: Set[ActorConfig],
            analyzer: ResilienceAnalysis,
            test: Set[ActorConfig] => Status,
            start: Long,
            timeout: Int,
            toSubsets: (Set[ActorConfig], Int) => List[Set[ActorConfig]] = DefaultSubsets): DDOutcome = {
        // DD algorithm
        def dd(cok: Set[ActorConfig], cnok: Set[ActorConfig], n: Int = 2): DDOutcome = {
            val diff = cnok.diff(cok)
            //debug(diff)

            // proceed if:
            //  we are in iteration > 0, thus we must have an error
            //  we are in iteration 0 (trying all faults)
            if((analyzer.exception.nonEmpty && analyzer.lastReport.exists(_.iteration > 0)) || (analyzer.exception.isEmpty && analyzer.lastReport.isEmpty)) {

                // Timeout
                if (System.currentTimeMillis() - start < timeout) {

                    // Invariant
                    if (!(test(cnok) == Status.Fail && test(cok) == Status.Pass && n <= diff.size)) {
                        debug(s"Invariant failed, solution: ($cok, $cnok)")
                        DDResult(cok, cnok)
                    } else {
                        val subsets: List[Set[ActorConfig]] = toSubsets(diff, n)

                        debug(s"Subsets: \n${subsets.mkString("\n")}")

                        // Does there exist a subset where: cok + subset == Fail
                        // E.g. [] + [1,2] => [1,2] are for sure bad elements because they fail
                        val case1 = subsets.find(s => test(cok.union(s)) == Status.Fail) // TODO: this can be parallelized

                        // Case 1: Reduce to subset
                        if (case1.nonEmpty) {
                            debug("Case1: reduce to subset")
                            // Yes, we continue with: cnok = union of cok and subset, because this reduces the faulty search space (↓)
                            // We are sure that the error is caused by only the subset or a combination of cok + subset
                            dd(cok, cok.union(case1.get), 2)
                        } else {
                            // Does there exist a subset where: cnok - subset == Pass
                            // E.g. [1,2,3,4] - [3,4,5] = [1,2] are for sure good elements because they pass
                            val case2 = subsets.find(s => test(cnok.diff(s)) == Status.Pass)

                            // Case 2: Increase to complement
                            if (case2.nonEmpty) {
                                debug("Case 2: Increase to complement")
                                // Yes, we continue with: cok = difference of cnok and subset, because this increases the correct search space (↑)
                                // Elements in subset do not cause failure
                                dd(cnok.diff(case2.get), cnok, 2)
                            } else {
                                // Does there exist a subset where: cok + s == Pass
                                // E.g. [1, 2] + [1,2,3,4] = [1,2,3,4] are for sure good elements because they pass
                                val case3 = subsets.find(s => test(cok.union(s)) == Status.Pass)

                                // Case 3: Increase to subset
                                if (case3.nonEmpty) {
                                    debug("Case 3: Increase to subset")
                                    // Yes, we continue with: cok = cok + subset, because this increases the correct search space (↑)
                                    // Elements in subset do not cause failure
                                    dd(cok.union(case3.get), cnok, Math.max(n - 1, 2))
                                } else {
                                    // Does there exist a where: cnok - s == Fail
                                    // E.g. [1,2,3,4] - [1,2] = [3,4] are for sure bad elements because they fail
                                    val case4 = subsets.find(s => test(cnok.diff(s)) == Status.Fail)

                                    // Case 4: Reduce to complement
                                    if (case4.nonEmpty) {
                                        debug("Case 4: Reduce to complement")
                                        // Yes, we continue with: cnok = cnok - subset, because this reduces the faulty search space (↓)
                                        // Elements in the complement cause a failure
                                        dd(cok, cnok.diff(case4.get), Math.max(n - 1, 2))
                                    } else {

                                        // Case 5: Increase granularity
                                        // If all tests turn out to be unresolved
                                        if (n < diff.size) {
                                            debug("Case 5: Increase granularity")
                                            dd(cok, cnok, Math.min(2 * n, diff.size))
                                        } else {
                                            // Case 6: We got the solution
                                            debug("Case 6: Result")
                                            DDResult(cok, cnok)
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    DDTimeout
                }
            } else {
                //
                DDNoErrorFound
            }
        }

        dd(Set.empty[ActorConfig], cnok)
    }

    def dd[A](c: Set[A], test: Set[A] => Status): Set[A] = {

        def dd2(c: Set[A], r: Set[A] = Set.empty[A]): Set[A] = {
            val (c1, c2) = c.splitAt(c.size / 2)

            debug(s"(0) Split:\nc:$c\nr:$r\nc1:$c1\nc2:$c2")

            require(test(r) == Status.Pass && test(c.union(r)) == Status.Fail, s"test(r)=${test(r)}, test(c.union(r))=${test(c.union(r))}")

            if(c.size == 1) {
                debug(s"(1) c.size == 1: $c")
                c
            } else if(test(c1.union(r)) == Status.Fail) {
                debug(s"(2) test(c1.union(r)) == Status.Fail:\nc1:$c1\nr:$r\nunion:${c1.union(r)}")
                dd2(c1, r)
            } else if(test(c2.union(r)) == Status.Fail) {
                debug(s"(3) test(c2.union(r)) == Status.Fail:\nc2:$c2\nr:$r\nunion:${c2.union(r)}")
                dd2(c2, r)
            } else {
                debug(s"(4) else")
                dd2(c1, c2.union(r)).union(dd2(c2, c1.union(r)))
            }
        }

        dd2(c)
    }

    def ddg[A](cnok: Set[A], test: Set[A] => Status): (Set[A], Set[A]) = {

        def dd(cok: Set[A], cnok: Set[A], n: Int = 2): (Set[A], Set[A]) = {
            val diff = cnok.diff(cok)
            println(diff)

            // Invariant
            if(!(test(cnok) == Status.Fail && test(cok) == Status.Pass && n <= diff.size)) {
                debug(s"Invariant failed, solution: ($cok, $cnok)")
                (cok, cnok)
            } else {
                val subsets: List[Set[A]] = diff.grouped(diff.size / n).toList //

                println(s"Subsets: $subsets")

                //require(n <= diff.size, s"size was $n <= ${diff.size}")
                //require(test(cnok) == Status.Fail, s"test(cnok)=${test(cnok)}")
                //require(test(cok) == Status.Pass, s"test(cok)=${test(cok)}")

                // Does there exist a subset where: cok + subset == Fail
                // E.g. [] + [1,2] => [1,2] are for sure bad elements because they fail
                val case1 = subsets.find(s => test(cok.union(s)) == Status.Fail) // TODO: this can be parallelized

                // Case 1: Reduce to subset
                if (case1.nonEmpty) {
                    debug("Case1: reduce to subset")
                    // Yes, we continue with: cnok = union of cok and subset, because this reduces the faulty search space (↓)
                    // We are sure that the error is caused by only the subset or a combination of cok + subset
                    dd(cok, cok.union(case1.get), 2)
                } else {
                    // Does there exist a subset where: cnok - subset == Pass
                    // E.g. [1,2,3,4] - [3,4,5] = [1,2] are for sure good elements because they pass
                    val case2 = subsets.find(s => test(cnok.diff(s)) == Status.Pass)

                    // Case 2: Increase to complement
                    if (case2.nonEmpty) {
                        debug("Case 2: Increase to complement")
                        // Yes, we continue with: cok = difference of cnok and subset, because this increases the correct search space (↑)
                        // Elements in subset do not cause failure
                        dd(cnok.diff(case2.get), cnok, 2)
                    } else {
                        // Does there exist a subset where: cok + s == Pass
                        // E.g. [1, 2] + [1,2,3,4] = [1,2,3,4] are for sure good elements because they pass
                        val case3 = subsets.find(s => test(cok.union(s)) == Status.Pass)

                        // Case 3: Increase to subset
                        if (case3.nonEmpty) {
                            debug("Case 3: Increase to subset")
                            // Yes, we continue with: cok = cok + subset, because this increases the correct search space (↑)
                            // Elements in subset do not cause failure
                            dd(cok.union(case3.get), cnok, Math.max(n - 1, 2))
                        } else {
                            // Does there exist a where: cnok - s == Fail
                            // E.g. [1,2,3,4] - [1,2] = [3,4] are for sure bad elements because they fail
                            val case4 = subsets.find(s => test(cnok.diff(s)) == Status.Fail)

                            // Case 4: Reduce to complement
                            if (case4.nonEmpty) {
                                debug("Case 4: Reduce to complement")
                                // Yes, we continue with: cnok = cnok - subset, because this reduces the faulty search space (↓)
                                // Elements in the complement cause a failure
                                dd(cok, cnok.diff(case4.get), Math.max(n - 1, 2))
                            } else {

                                // Case 5: Increase granularity
                                // If all tests turn out to be unresolved
                                if (n < diff.size) {
                                    debug("Case 5: Increase granularity")
                                    dd(cok, cnok, Math.min(2 * n, diff.size))
                                } else {
                                    // Case 6: We got the solution
                                    debug("Case 6: Result")
                                    (cok, cnok)
                                }
                            }
                        }
                    }
                }
            }
        }

        dd(Set.empty[A], cnok)
    }

}
