package Lab2

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound.CPD

object Ex1 {
	def main(args: Array[String]) {
		val reducere = Flip(0.20)
		val urgenta = Flip(0.05)
		val cumparat = CPD(reducere, urgenta,
			(true, true) -> Constant(true),
			(true, false) -> Flip(0.5),
			(false, true) -> Flip(0.8),
			(false, false) -> Flip(0.2)
		)

		cumparat.observe(true)
		val alg = VariableElimination(reducere, urgenta)
		alg.start()
		alg.stop()
		println("Probabilitatea reducere daca a cumparat " + alg.probability(reducere, true))
		println("Probabilitatea urgenta daca a cumparat " + alg.probability(urgenta, true))

		cumparat.unobserve()

		reducere.observe(false)
		urgenta.observe(false)
		val alg1 = VariableElimination(cumparat)
		alg1.start()
		alg1.stop()
		println("Probabilitate a cumparat daca nu e reducere sau urgenta " + alg1.probability(cumparat, true))
	}
}