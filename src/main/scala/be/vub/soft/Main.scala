package be.vub.soft

import be.vub.soft.analysis.{DDOptimalResilienceAnalysis, DDResilienceAnalysis, OverheadAnalysis, RandomResilienceAnalysis}
import be.vub.soft.perturbations.{AtLeastOnceDeliveryDuplication, PersistentActorRestart}
import be.vub.soft.runner.{ResilienceAnalysisRunner, ResilienceAnalysisRunnerPaper}

object Main {

    var TIMEOUT = (60 * 60 * 1000 * 0.5).toInt // 30 mins
    val Runs = 1

    def main(args: Array[String]): Unit = {
        if (args.isEmpty) {
            println("Missing argument: the path of the system to analyze")
            System.exit(0)
        } else if (args(0).contains(" ")) {
            println("Target path cannot contain spaces")
            System.exit(0)
        } else {
            println(s"Analyzing ${args(0)}")

            val suite = if(args.length > 1) Some(args(1)) else None
            val test = if(args.length > 2) Some(args(2)) else None

            // sbt "run examples/SystemUnderTest2"
            // We are searching for defects in persistence recovery in this example
            // The solution will be:
            // defect in ActorC after message Update from ActorB with hashcode 266369878
            val runner = new ResilienceAnalysisRunner(args(0), suite, test)
            runner.initialize()
            runner.analyse(test => new DDOptimalResilienceAnalysis(PersistentActorRestart), Runs)

            // Paper evaluation, run either for performance or overhead
            // sbt "run examples/Evaluation"
            // The test name indicates which defects the analysis searches for.
            /*
                // Performance
                val runner = new ResilienceAnalysisRunnerPaper(args(0), suite, test)
                runner.initialize()

                runner.analyse(perturbation => new DDOptimalResilienceAnalysis(perturbation), Runs)
                runner.analyse(perturbation => new DDResilienceAnalysis(perturbation), Runs)
                runner.analyse(perturbation => new RandomResilienceAnalysis(perturbation), Runs)

                // Overhead
                for(i <- 0 to 1000 by 100) runner.analyse(perturbation => new OverheadAnalysis(i, PersistentActorRestart), Runs)
                for(i <- 0 to 1000 by 100) runner.analyse(perturbation => new OverheadAnalysis(i, AtLeastOnceDeliveryDuplication), Runs)
             */
        }
    }

}