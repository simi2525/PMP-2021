package Partial1

import com.cra.figaro.language.{Element, Constant, Apply, Flip, Chain}	
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance

object Barbut {

    val zar1 = Uniform(1,2,3,4,5,6)
    val zar2 = Uniform(1,2,3,4,5,6)
    val throwdice = Apply(zar1, zar2, 
        (p1: Int, p2: Int) => p1 + p2
    )
    
    def play(p1Wins: Element[Int], 
             p2Wins: Element[Int], 
             no: Int) : Element[Int] =
    {
        if (no == 0)
            Apply(p1Wins, p2Wins, (n1: Int, n2: Int) => 
                    if (n1 > n2) 1 
                    else if (n2 > n1) 2
                    else 0
                 )
        else
            Chain(throwdice, (s: Int) =>
                if (s == 7 || s == 11) {
                    val p1NewWins = Apply(p1Wins, (n: Int) => n+1)
                    play(p1NewWins, p2Wins, no - 1)
                }
                else if (s == 2 || s == 3 || s == 12) {
                    val p2NewWins = Apply(p2Wins, (n: Int) => n+1)
                    play(p1Wins, p2NewWins, no - 1)
                } 
                else play(p1Wins, p2Wins, no - 1)
            )
    }
    
    def main(args: Array[String]) {	

        println("Probab. p1 castiga: " + 
            VariableElimination.probability(throwdice, (s: Int) => s == 7 || s == 11))
    
        println("Probab. p2 castiga: " + 
            VariableElimination.probability(throwdice, (s: Int) => s == 2 || s== 3 || s == 12))
        /*
        Rezultate afisate:
        Probab. p1 castiga: 0.2222222222222222
        Probab. p2 castiga: 0.1111111111111111
        Probabibilitatea pentru p1 este dubla deoarece sunt 6 cazuri favorabile, 
        iar pentru al doilea doar 3.
        */
        val barbut = play(Constant(0), Constant(0), 10)
        val alg = Importance(10000, barbut)
        alg.start()
        println("Primul castiga cu probab: " + 
            alg.probability(barbut, 1))
        println("Al doilea castiga cu probab: " + 
            alg.probability(barbut, 2)) 
        /*
        Rezultate afisate:
        Primul castiga cu probab: 0.22280000000000486
        Al doilea castiga cu probab: 0.10980000000000174
        Sunt aproximari ale probabilitatilor de castig pentru fiecare. 
        Aceste probabilitati nu depind de numarul de ture jucate.
        */
        println("Este remiza cu probab: " + 
            alg.probability(barbut, 0)) 

    }
}