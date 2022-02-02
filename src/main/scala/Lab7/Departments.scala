package Lab7

import com.cra.figaro.language.{Element, Select, Flip, Apply, Chain}
import com.cra.figaro.library.compound.{^^}
import com.cra.figaro.algorithm.factored.VariableElimination

object Departments {

	class ResearchAndDevelopment {
		// val state = 
	}

	class HumanResources {
		// val state = 
	}

	class Production(val rd: ResearchAndDevelopment, val hr: HumanResources) {
		// val state = 
	}

	class Sales(val p: Production) {
		// val state = 
	}

	class Finance(val hr: HumanResources, val s: Sales) {
		// val state = 
	}

	class Firm(val rd: ResearchAndDevelopment, val hr: HumanResources, val p: Production, val s: Sales, val f: Finance) {
		// val health = 
	}

	def main(args: Array[String]) {
		val rd = new ResearchAndDevelopment()
		val hr = new HumanResources()
		val p = new Production(rd, hr)
		val s = new Sales(p)
		val f = new Finance(hr, s)
		val firm = new Firm(rd, hr, p, s, f)

		
	}
}