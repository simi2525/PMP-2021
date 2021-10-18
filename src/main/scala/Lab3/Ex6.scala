package Lab3 

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex6 {
	def main(args: Array[String]) {
		val die1 = FromRange(1, 7)
		val die2 = FromRange(1, 7)
		val die3 = FromRange(1, 7)
		val die4 = FromRange(1, 7)
		val die5 = FromRange(1, 7)
		val die6 = FromRange(1, 7)

		val double1 = Apply(die1, die2, (die1: Int, die2: Int) => die1 == die2)
		val double2 = Apply(die3, die4, (die3: Int, die4: Int) => die3 == die4)
		val double3 = Apply(die5, die6, (die5: Int, die6: Int) => die5 == die6)

		val jail = Apply(double1, double2, double3, (d1: Boolean, d2: Boolean, d3: Boolean) => d1 && d2 && d3)

		val doubles = Apply(die1, die3, die5, (d1: Int, d2: Int, d3: Int) => (d1, d2, d3))
		doubles.addCondition((doubles: (Int, Int, Int)) => doubles._1 != doubles._2 && doubles._2 != doubles._3 && doubles._1 != doubles._3)

		println(VariableElimination.probability(jail, true))
	}
}