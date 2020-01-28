# Chaokka
*Chaokka* is a tool for testing the resilience of Akka actor systems to be used during development with existing test suites. Resilient systems can withstand temporal adverse conditions such as messages that are lost or actors that crash.

To assess the resilience of a system, Chaokka perturbs the system with perturbations regarding at-least-once message delivery issues and persistence issues. In particular, it focusses on duplicated messages due to at-least-once message delivery mechanism, as well as wrong state persistence/hydratation due to event sourcing.

In general, it is infeasible to manually explore all the messages and actors in these adverse conditions. Therefore, Chaokka automatically pertubs the system and can do this in a naive and random way or in more efficient ways by leveraging delta-debugging and causality of actors.

Chaokka is build using AspectJ to intercept framework calls of Akka, to monitor the intercepted messages and actors, and to perturb the system with perturbations.

The output will be saved as a csv and text file in the directory `/csv`.

## Usage
To use Chaokka, you only have to specify a path to the target system. We advise you to use Akka 2.5.23 and ScalaTest 3.0.5 in these systems.

The exploration strategy, timeout and iterations can be configured in `Main.scala`.
The exploration strategies can be found in `src/main/scala/be/vub/soft/analysis`.
Two examples can be found in the `/examples` directory.

For instance, to asses the resilience of the second example:
> sbt "run examples/SystemUnderTest2"

## Limitations
As Chaokka is a research prototype, it does not work on every system and test suite:
- Only works for actor systems that are deterministic in the message and actors it creates.
- Only local actor systems can be used. However, due to Akka's location transparency this should not be an issue to reconfigure.
- Every actor needs a unique name.
- Tests are expected to follow a format where:
-- one message is sent to trigger the execution of the system
-- a blocking operation to wait until the system is done
-- one or more assertions that asserts the state of the system

