package Lab4

import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.language.Chain
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}
import com.cra.figaro.language.{Flip, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.^^

object Ex4TwoCards {
  def main(args: Array[String])
  {
    val cards = List(5, 4, 3, 2, 1)

    val player1Card1 = discrete.Uniform(cards:_*)

    val player2Card1 = Chain(player1Card1,
        (card: Int) => discrete.Uniform(cards.filter(_ != card):_*)
    )

    val player1Card2 = Chain(player1Card1, player2Card1,
        (card1: Int, card2: Int) => discrete.Uniform(cards.filter(x=>(x != card1 && x != card2)):_*)
    )

    val playercards = ^^(player1Card1, player2Card1, player1Card2)
    
    val player2Card2 = Chain(playercards,
        (playercards:(Int, Int, Int))	=> discrete.Uniform(cards.filter(x=>(x != playercards._1 && x != playercards._2 && x != playercards._3)):_*)
    )


    val player1Bet1 = RichCPD(player1Card1, player1Card2,
        (OneOf(5, 4, 3), OneOf(5, 4, 3)) -> Flip(0.9),
        (*, *) -> Flip(0.4)
    )

    val player2Bet = RichCPD(player2Card1, player2Card2, player1Bet1,
        (OneOf(5, 4, 3), OneOf(5, 4, 3), *) -> Flip(0.9),
        (*, *, OneOf(false)) -> Flip(0.5),
        (*, *, *) -> Flip(0.1)
    )

    val player1Bet2 = Apply(player1Card1, player1Card2, player1Bet1, player2Bet,
        (card1: Int, card2: Int, bet11: Boolean, bet2: Boolean) =>
            !bet11 && bet2 && (card1 == 5 || card1 == 4 || card1 == 3) && (card2 == 5 || card2 == 4 || card2 == 3)
    )

    val player1Gain = Apply(^^(player1Card1, player1Card2, player2Card1, player2Card2), player1Bet1, player2Bet, player1Bet2,
        (pcards: (Int, Int, Int, Int), bet11: Boolean, bet2: Boolean, bet12: Boolean) =>
            if (!bet11 && !bet2) 0.0
            else if (bet11 && !bet2) 1.0
            else if (!bet11 && bet2 && !bet12) -1.0
            else if ((pcards._1 > pcards._3 || pcards._1 > pcards._4) && (pcards._2 > pcards._3 || pcards._2 > pcards._4)) 2.0
            else -2.0)

    player1Card1.observe(4)
    player1Card2.observe(5)
    player1Bet1.observe(true)
    val alg1 = VariableElimination(player1Gain)
    alg1.start()
    alg1.stop()
    println("Expected gain for betting:" + alg1.mean(player1Gain))

    player1Bet1.observe(false)
    val alg2 = VariableElimination(player1Gain)
    alg2.start()
    alg2.stop()
    println("Expected gain for passing:" + alg2.mean(player1Gain))
    player1Card1.unobserve()
    player1Card2.unobserve()
    player1Bet1.unobserve()

    player2Card1.observe(5)
    player2Card2.observe(4)
    player1Bet1.observe(false)
    player2Bet.observe(true)
    val alg3 = VariableElimination(player1Gain)
    alg3.start()
    alg3.stop()
    println("Expected gain for betting:" + alg3.mean(player1Gain))

    player2Bet.observe(false)
    val alg4 = VariableElimination(player1Gain)
    alg4.start()
    alg4.stop()
    println("Expected gain for passing:" + alg4.mean(player1Gain))
  }
}