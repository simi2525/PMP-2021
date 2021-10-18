package Lab3 

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex4 {
	def main(args: Array[String]) {
		val die1 = FromRange(1, 7)
		val die2 = FromRange(1, 7)
		val die3 = FromRange(1, 7)
		val total = Apply(die1, die2, die3, (i1: Int, i2: Int, i3: Int) => i1 + i2 + i3)
		println(VariableElimination.probability(total, 11))
	}
}