package Lab3 

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3 {
	def main(args: Array[String])
	{
		val x1 = Flip(0.4)
		val y1 = Flip(0.4)
		val z1 = x1
		val w1 = x1 === z1
		println(VariableElimination.probability(w1, true))

		val x2 = Flip(0.4)
		val y2 = Flip(0.4)
		val z2 = y2
		val w2 = x2 === z2
		println(VariableElimination.probability(w2, true))
	}
}