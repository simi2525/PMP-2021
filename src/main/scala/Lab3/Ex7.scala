package Lab3 

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex7 {
	def main(args: Array[String]) {
		val numberOfSides = Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
		val roll = Chain(numberOfSides, ((i: Int) => FromRange(1, i + 1)))

		// a
		println(VariableElimination.probability(numberOfSides, 12))

		//b
		println(VariableElimination.probability(roll, 7))

		//c
		roll.observe(7)
		println(VariableElimination.probability(numberOfSides, 12))
		roll.unobserve()

		// d
		numberOfSides.observe(12)
		println(VariableElimination.probability(roll, 7))
	}
}