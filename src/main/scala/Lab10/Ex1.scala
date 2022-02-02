package lab10

import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex5 {
  def main(args: Array[String]) {

    // pt varianta 1 si 2

    val months = 13
    val fraction = 0.3

    var investment: Array[Element[Double]] = Array.fill(months)(Constant(0.0))
    var profit: Array[Element[Double]] = Array.fill(months)(Constant(0.0))
    var capital: Array[Element[Double]] = Array.fill(months)(Constant(0.0))

    capital(0) = Constant(1200.0)

    // varianta 1

    for {month <- 1 until months} {
      investment(month) = Apply(capital(month - 1), (cap: Double) => cap * fraction) // investeste cu 30 % din capitalul anterior

      profit(month) = Chain(investment(month), capital(month - 1), (inv: Double, cap: Double) =>
        if (inv >= 0.5 * cap) Select(0.1 -> (0.4 * cap), 0.3 -> (0.5 * cap), 0.6 -> (0.7 * cap)); // daca investitia este mai mare de 50 % din capital atunci va avea un profit de 70 % din capital cu probabilitatea de 0.6
        else if (inv >= 0.3 * cap) Select(0.2 -> (0.25 * cap), 0.6 -> (0.5 * cap), 0.2 -> (0.35 * cap)); // daca investitia este mai mare de 30 % din capital atunci va avea un profit de 50 % din capital cu probabilitatea de 0.6
        else Select(0.6 -> (0.3 * cap), 0.3 -> (0.2 * cap), 0.1 -> (0.1 * cap))) // daca investitia este mai mica de 30 % din capital atunci va avea un profit de 30 % din capital cu probabilitatea de 0.6
      capital(month) = Apply(profit(month), capital(month - 1), investment(month),
        (prof: Double, cap: Double, invest: Double) => cap + prof - invest)
    }

    println(Importance.probability(capital(10), (c: Double) => c > 1200.0))




    // varianta 2

//    def transition(month: Int): (Element[(Double, Double, Double)]) = {
//      val cap = capital(month - 1)
//      val newInvestment = Apply(cap, (cap: Double) => cap * fraction)
//
//      val newProfit = Chain(newInvestment, cap, (inv: Double, cap: Double) =>
//        if (inv >= 50.0 * cap / 100.0) Select(0.1 -> ((10.0 / 100.0) * cap), 0.3 -> ((20.0 / 100.0) * cap), 0.6 -> ((70.0 / 100.0) * cap));
//        else if (inv >= 30.0 * cap / 100.0) Select(0.2 -> ((35.0 / 100.0) * cap), 0.6 -> ((60.0 / 100.0) * cap), 0.2 -> ((35.0 / 100.0) * cap));
//        else Select(0.6 -> ((20.0 / 100.0) * cap), 0.3 -> ((30.0 / 100.0) * cap), 0.1 -> ((40.0 / 100.0) * cap)))
//      val newCapital = Apply(newProfit, cap, newInvestment,
//        (prof: Double, cap: Double, invest: Double) => cap + prof - invest)
//      ^^(newInvestment, newProfit, newCapital)
//    }
//    for {month <- 1 until months} {
//      val newState = transition(month)
//      investment(month) = newState._1
//      profit(month) = newState._2
//      capital(month) = newState._3
//    }
//
//    println(Importance.probability(capital(10), (c: Double) => c > 1200.0))





    // varianta 3


//    val initial = Universe.createNew()
//    Constant(1200.0)("capital", initial)
//
//    def transition(cap: Double): (Element[(Double, Double, Double)]) = {
//      val capital = Constant(cap)
//      val newInvestment = Apply(capital, (cap: Double) => cap * fraction)
//
//      val newProfit = Chain(newInvestment, capital, (inv: Double, cap: Double) =>
//        if (inv >= 0.5 * cap) Select(0.1 -> (0.4 * cap), 0.3 -> (0.5 * cap), 0.6 -> (0.7 * cap));
//        else if (inv >= 0.3 * cap) Select(0.2 -> (0.25 * cap), 0.6 -> (0.5 * cap), 0.2 -> (0.35 * cap));
//        else Select(0.6 -> (0.3 * cap), 0.3 -> (0.2 * cap), 0.1 -> (0.1 * cap)))
//      val newCapital = Apply(newProfit, capital, newInvestment,
//        (prof: Double, cap: Double, invest: Double) => cap + prof - invest)
//      ^^(newInvestment, newProfit, newCapital)
//    }
//
//    def nextUniverse(previous: Universe): Universe = {
//      val next = Universe.createNew()
//      val previousCapital = previous.get[Double]("capital")
//
//      val newState = Chain(previousCapital, transition _)
//      Apply(newState, (s: (Double, Double, Double)) => s._1)("investment", next)
//      Apply(newState, (s: (Double, Double, Double)) => s._2)("profit", next)
//      Apply(newState, (s: (Double, Double, Double)) => s._3)("capital", next)
//      next
//    }
//
//    val alg = ParticleFilter(initial, nextUniverse, 10000)
//    alg.start()
//
//    for {time <- 1 to 12} {
//      alg.advanceTime()
//      println("Time " + time + ": ")
//      println("expected profit = " + alg.currentExpectation("profit", (p: Double) => p))
//      println("expected investment = " + alg.currentExpectation("investment", (p: Double) => p))
//      println("expected capital = " + alg.currentExpectation("capital", (p: Double) => p))
//    }
  }
}
