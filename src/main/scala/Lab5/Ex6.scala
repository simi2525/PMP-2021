package lab5

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.{Apply, Constant, Element, Flip, Select, Chain}
import com.cra.figaro.library.compound.If

object ex6
{
    def main(args: Array[String])
    {

        def tennis(probP1ServeWin: Double, probP1Winner: Double, probP1out: Double, probP1net: Double,
                   probP2ServeWin: Double, probP2Winner: Double, probP2out: Double, probP2net: Double, referee: Double): Element[Boolean] = 
        {
            def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = 
            {
                val pWinner = if (firstShot && player1) probP1ServeWin
                              else if (firstShot && !player1) probP2ServeWin
                                    else if (player1) probP1Winner
                                         else probP2Winner

                val refereeIsWrong = Flip(referee)
                val error = if (player1) Select(probP1out-> "out", probP1net-> "net", 1-probP1out-probP1net-> "court")
                            else  Select(probP2out-> "out", probP2net-> "net", 1-probP2out-probP2net-> "court")
                val winner = Chain(error, refereeIsWrong,
                                   (e: String, r: Boolean) => 
                                   if (r == true)
                                        if (e == "court") Constant(!player1) 
                                        else if (e == "out") Constant(player1)
                                        else Constant(player1)
                                   else 
                                        Flip(pWinner))
                val pError = Chain(error, (e: String) => if (error == "out" || error == "net") Constant(true)
                                                        else Constant(false))


                If(winner,
                   Constant(player1),
                   If(pError, Constant(!player1), rally(false, !player1)))
            }

            def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): Element[Boolean] =
            {
                val p1WinsPoint = rally(true, p1Serves)

                val newP1Points = Apply(p1WinsPoint, p1Points,
                                        (wins: Boolean, points: Int) =>
                                        if (wins) points + 1 else points)

                val newP2Points = Apply(p1WinsPoint, p2Points,
                                        (wins: Boolean, points: Int) =>
                                        if (wins) points else points + 1)

                val p1WinsGame = Apply(newP1Points, newP2Points,
                                       (p1: Int, p2: Int) =>
                                       p1 >= 4 && p1 - p2 >= 2)

                val p2WinsGame = Apply(newP2Points, newP1Points,
                                       (p2: Int, p1: Int) =>
                                       p2 >= 4 && p2 - p1 >= 2)

                val gameOver = p1WinsGame || p2WinsGame

                If(gameOver, p1WinsGame, game(p1Serves, newP1Points, newP2Points))
            }

            def play(p1Serves: Boolean, p1Sets: Element[Int], p2Sets: Element[Int],
                     p1Games: Element[Int], p2Games: Element[Int]): Element[Boolean] =
            {
                val p1WinsGame = game(p1Serves, Constant(0), Constant(0))
                val newP1Games = Apply(p1WinsGame, p1Games, p2Games,
                                       (wins: Boolean, p1: Int, p2: Int) =>
                                        if (wins)
                                        {
                                            if (p1 >= 5) 0 else p1 + 1
                                        }
                                        else
                                        {
                                            if (p2 >= 5) 0 else p1
                                        })

                val newP2Games = Apply(p1WinsGame, p1Games, p2Games,
                                       (wins: Boolean, p1: Int, p2: Int) =>
                                        if (wins)
                                        {
                                            if (p1 >= 5) 0 else p2
                                        }
                                        else
                                        {
                                            if (p2 >= 5) 0 else p2 + 1
                                        })

                val newP1Sets = Apply(p1WinsGame, p1Games, p1Sets,
                                      (wins: Boolean, games: Int, sets: Int) =>
                                       if (wins && games == 5)
                                            sets + 1
                                       else 
                                            sets)

                val newP2Sets = Apply(p1WinsGame, p2Games, p2Sets,
                                      (wins: Boolean, games: Int, sets: Int) =>
                                      if (!wins && games == 5)
                                            sets + 1
                                      else
                                            sets)

                val matchOver = Apply(newP1Sets, newP2Sets,
                                      (p1: Int, p2: Int) =>
                                      p1 >= 2 || p2 >= 2)
                
                If(matchOver,
                   Apply(newP1Sets, (sets: Int) => sets >= 2),
                   play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games))
            }

            play(true, Constant(0), Constant(0), Constant(0), Constant(0))
        }

        val tennis_match = tennis(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.9, 0.5, 0.1)
        val alg = Importance(1000, tennis_match)
        alg.start()
        alg.stop()
        println("Expected gain:" + alg.probability(tennis_match, true))
    }
}