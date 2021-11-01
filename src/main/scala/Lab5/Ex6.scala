package Lab5

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Apply, Constant, Element, Flip}
import com.cra.figaro.library.compound.If

object Ex6
{
	def tennis(probP1ServeWin: Double, probP1Winner: Double, probP1Error: Double, probP2ServeWin: Double, probP2Winner: Double, probP2Error: Double): Element[Boolean] = 
	{
		def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = 
		{
			val pWinner = if (firstShot && player1) probP1ServeWin
							else if (firstShot && !player1) probP2ServeWin
								else if (player1) probP1Winner
										else probP2Winner
			
			val pError = if (player1) probP1Error else probP2Error
			val winner = Flip(pWinner)
			val error = Flip(pError)
			If(winner,
				Constant(player1),
				If(error, 
					Constant(!player1), 
					rally(false, !player1)
				)
			)
		}

		def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): Element[Boolean] =
		{
			val p1WinsPoint = rally(true, p1Serves)

			val newP1Points = Apply(p1WinsPoint, p1Points,
								(wins: Boolean, points: Int) =>
									if (wins) points + 1 else points
			)

			val newP2Points = Apply(p1WinsPoint, p2Points,
								(wins: Boolean, points: Int) =>
									if (wins) points else points + 1
			)

			val p1WinsGame = Apply(newP1Points, newP2Points,
								(p1: Int, p2: Int) =>
									p1 >= 4 && p1 - p2 >= 2
			)

			val p2WinsGame = Apply(newP2Points, newP1Points,
								(p2: Int, p1: Int) =>
									p2 >= 4 && p2 - p1 >= 2
			)

			val gameOver = p1WinsGame || p2WinsGame

			If(gameOver, 
				p1WinsGame, 
				game(p1Serves, newP1Points, newP2Points)
			)
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
									}
			)

			val newP2Games = Apply(p1WinsGame, p1Games, p2Games,
								(wins: Boolean, p1: Int, p2: Int) =>
									if (wins)
									{
										if (p1 >= 5) 0 else p2
									}
									else
									{
										if (p2 >= 5) 0 else p2 + 1
									}
			)

			val newP1Sets = Apply(p1WinsGame, p1Games, p1Sets,
								(wins: Boolean, games: Int, sets: Int) =>
									if (wins && games == 5)
										sets + 1
									else 
										sets
			)

			val newP2Sets = Apply(p1WinsGame, p2Games, p2Sets,
								(wins: Boolean, games: Int, sets: Int) =>
									if (!wins && games == 5)
										sets + 1
									else
										sets
			)

			val matchOver = Apply(newP1Sets, newP2Sets,
								(p1: Int, p2: Int) =>
									p1 >= 2 || p2 >= 2
			)
			
			If(matchOver,
				Apply(newP1Sets, (sets: Int) => sets >= 2),
				play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games)
			)
		}

		play(true, Constant(0), Constant(0), Constant(0), Constant(0))
	}


	def main(args: Array[String])
	{
		val tennis_match = tennis(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
		val alg = Importance(200, tennis_match)
		alg.start()
		alg.stop()
		println("Expected gain:" + alg.probability(tennis_match, true))
	}
}