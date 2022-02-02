package Lab13

import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Flip, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.{VariableElimination, MPEVariableElimination}
import com.cra.figaro.algorithm.sampling.{Importance, MetropolisHastings, ProposalScheme, MetropolisHastingsAnnealer, Schedule}
import com.cra.figaro.algorithm.factored.beliefpropagation.{BeliefPropagation, MPEBeliefPropagation}
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.OneTimeMPE

object lab13
{
	abstract class State
	{
		val confident: Element[Boolean]
		def possession: Element[Boolean] = If(confident, Flip(0.7), Flip(0.3))
	}
	
	class InitialState() extends State
	{
		val confident = Flip(0.4)
	}

	class NextState(current: State) extends State
	{
		val confident =	If(current.confident, Flip(0.6), Flip(0.3))
	}
	
	def stateSequence(n: Int): List[State] =
	{
		if (n == 0)
			List(new InitialState())
		else
		{
			val last :: rest = stateSequence(n - 1)
			new NextState(last) :: last :: rest
		}
	}
	
	def run(obsSeq: List[Boolean], algorithm:OneTimeMPE)
	{
		val stateSeq = stateSequence(obsSeq.length)
		for { i <- 0 until obsSeq.length }
		{
			stateSeq(i).possession.observe(obsSeq(obsSeq.length - 1 - i))
		}

		algorithm.start()
		for { i <- 0 until stateSeq.length - 1 }
		{
			print(obsSeq(obsSeq.length - 1 - i))
			print("\t")
			println(algorithm.mostLikelyValue(stateSeq(stateSeq.length - 1 - i).confident))
		}

		algorithm.kill()
	}


	def main(args: Array[String])
	{
		var steps = 10
		var obsSeq = List.fill(steps)(scala.util.Random.nextBoolean())

		println("MPE variable elimination")
		run(obsSeq, MPEVariableElimination())
		println("MPE belief propagation")
		run(obsSeq, MPEBeliefPropagation(10))
		println("Simulated annealing")
		run(obsSeq, MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)))


	}
}