import complex.Complex._
import complex.ComplexVector

object Main extends App {
    for (y <- -1 to 1; x <- -1 to 1) {
        println(x + y * i)
    }
    
    println("*******")
    
    val v1 = ComplexVector(1, 2 - i, -5 * i, -3)
    println(v1)
    println(-v1)
    
    val c1 = 1 + 2 * i
    val c2 = 1 + 2 * i
    println(c1 == c2)
    
}
