package Lab3 

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.FromRange
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.^^

object Ex8 {
	def main(args: Array[String]) {
		val numberOfSides1 = Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
		val roll1 = Chain(numberOfSides1, ((i: Int) => FromRange(1, i + 1)))

		val numberOfSides2 = Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
		val roll2 = Chain(numberOfSides2, ((i: Int) => FromRange(1, i + 1)))

		def stickinessConstraint(sidesPair: (Int, Int)) = if (sidesPair._1 == sidesPair._2) 1.0 else 0.5

		val pairOfSides = ^^(numberOfSides1, numberOfSides2)

		pairOfSides.addConstraint(stickinessConstraint)
		println(VariableElimination.probability(roll2, 7))

		roll1.observe(7)
		println(VariableElimination.probability(roll2, 7))
	}
}