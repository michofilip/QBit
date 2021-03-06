package complex

import complex.Complex._

import scala.language.implicitConversions

class Complex(val re: Double, val im: Double) {
  def unary_+ : Complex = Complex(re, im)

  def unary_- : Complex = Complex(-re, -im)

  def +(that: Complex): Complex = Complex(this.re + that.re, this.im + that.im)

  def -(that: Complex): Complex = this + -that

  def *(that: Complex): Complex = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)

  def /(that: Complex): Complex = this * inverse(that)

  override def toString: String = {
    if (re > 0 && im > 0) {
      f"$re%.3f+$im%.3fi".replace(',', '.')
    } else if (re > 0 && im < 0) {
      f"$re%.3f$im%.3fi".replace(',', '.')
    } else if (re > 0 && im == 0) {
      f"$re%.3f".replace(',', '.')
    } else if (re < 0 && im > 0) {
      f"$re%.3f+$im%.3fi".replace(',', '.')
    } else if (re < 0 && im < 0) {
      f"$re%.3f$im%.3fi".replace(',', '.')
    } else if (re < 0 && im == 0) {
      f"$re%.3f".replace(',', '.')
    } else if (re == 0 && im > 0) {
      f"$im%.3fi".replace(',', '.')
    } else if (re == 0 && im < 0) {
      f"$im%.3fi".replace(',', '.')
    } else {
      "0.000"
    }
  }
}

object Complex {
  def i: Complex = Complex(0, 1)

  def expI(x: Double): Complex = Math.cos(x) + Math.sin(x) * i

  def apply(re: Double, im: Double): Complex = new Complex(re, im)

  implicit def double2Complex(x: Double): Complex = Complex(x, 0)

  implicit def int2Complex(x: Int): Complex = Complex(x, 0)

  def conjugate(c: Complex): Complex = Complex(c.re, -c.im)

  def modSqr(c: Complex): Double = c.re * c.re + c.im * c.im

  def mod(c: Complex): Double = Math.sqrt(modSqr(c))

  def inverse(c: Complex): Complex = conjugate(c) * (1 / modSqr(c))
}
