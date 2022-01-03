package Lab9

import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex5 {
  def main(args: Array[String]) {


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
  }
}