package Lab3 

import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex1 {
	val sunnyToday = Flip(0.2)

	val greetingToday = If(sunnyToday,
		Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
		Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again!")
	)

	val bedSide = Flip(0.4)

	val greeting = Chain(bedSide, (side: Boolean) =>
		if (!side) {
			Constant("Oh no, not again!")
		}
		else
			greetingToday
	)

	def main(args: Array[String]) {
		val result = VariableElimination.probability(greeting, "Oh no, not again!")
		println("Today’s greeting \"Oh no, not again!\" " + result)

		greeting.observe("Oh no, not again!")
		println(VariableElimination.probability(sunnyToday, true))
	}
}