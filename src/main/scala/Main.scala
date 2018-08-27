import complex.Complex
import complex.Complex._

object Main extends App {
  for (y <- -1 to 1; x <- -1 to 1) {
    val c = Complex(x, y)
    println(x + y * i)
  }

//  val c = 1 + 1 * i
//  println(expI(Math.PI / 2))

  //  println(Complex(3, 4))
  //  println(Complex(3, -4))
  //  println(Complex(-3, 4))
  //  println(Complex(-3, -4))
}
