package Lab2

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._


object Ex1Wrong{
    def main(args: Array[String]){
        val p1 = Flip(1) // Reducere + Urgeneta
        val p2 = Flip(0.5) // Reducere
        val p3 = Flip(0.8) // Urgenta
        val p4 = Flip(0.2) // Nimic

        val result = CPD (p1,p2, p3, p4, 
            (true, false, true, false) -> Constant("True"),
            (false, true, false, true) -> Constant("False"),
            // (true, true, true, true) -> Constant("True"),
            // (false, false, false, false) -> Constant("False"),
            // (true, false, false, true) -> Constant("True"),
            // (false, true, true, false) -> Constant("False"),
            // (true, true, false, false) -> Constant("True"),
            // (false, false, true, true) -> Constant("False"),
            // (false, true, true, true) -> Constant("True"),
            // (false, false, false, true) -> Constant("False"),
            // (true, true, true, false) -> Constant("True"),
            // (true, false, false, false) -> Constant("True"),
            // (false, false, true, false) -> Constant("False"),
            // (true, true, false, true) -> Constant("True"),
            // (false, true, false, false) -> Constant("False"),
            // (true, false, true, true) -> Constant("True"),
            
        )

        val algorithm = Importance(1000, result,p1,p3)
        algorithm.start()
        println(algorithm.probability( p1,true))
        println(algorithm.probability( p3,true))
    }
}