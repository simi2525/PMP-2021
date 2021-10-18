/*
 * Test.scala 
 * A simple Figaro test program.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com)
 * Creation Date:   Aug 6, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.sampling._

object Test {
	def main(args: Array[String]) {
		// Model
		val coin1 = Flip(0.25)
		val coin2 = Flip(0.7)
		val result = CPD(coin1,coin2,
			(true, true) -> Constant("HH"),
			(true, false) -> Constant("HT"),
			(false, true) -> Constant("TH"),
			(false, false) -> Constant("TT")
		)
		
		result.observe("TT")
		// Algo Inferenta
		val algorithm = Importance(1000, coin2)
		algorithm.start()
		
		// Interogam modelul
		println(algorithm.probability(coin1, true))
	}
}