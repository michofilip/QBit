package complex

import complex.Complex._

import scala.language.implicitConversions

class Complex(val re: Double, val im: Double) {
    def unary_+ : Complex = this
    
    def unary_- : Complex = Complex(-re, -im)
    
    def +(that: Complex): Complex = Complex(this.re + that.re, this.im + that.im)
    
    def -(that: Complex): Complex = this + -that
    
    def *(that: Complex): Complex = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
    
    def /(that: Complex): Complex = this * that.inverse
    
    def conjugate: Complex = Complex(re, -im)
    
    def absSqr: Double = re * re + im * im
    
    def abs: Double = Math.sqrt(absSqr)
    
    def inverse: Complex = conjugate * (1 / absSqr)
    
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
    
    override def hashCode(): Int = 41 * re.hashCode() + im.hashCode()
    
    override def equals(obj: scala.Any): Boolean = obj match {
        case c: Complex => hashCode() == c.hashCode() && re == c.re && im == c.im
        case _ => false
    }
}

object Complex {
    def i: Complex = Complex(0, 1)
    
    def expI(x: Double): Complex = Math.cos(x) + Math.sin(x) * i
    
    def apply(re: Double, im: Double): Complex = new Complex(re, im)
    
    def apply(re: Double): Complex = new Complex(re, 0)
    
    def unapply(arg: Complex): Option[(Double, Double)] = Some((arg.re, arg.im))
    
    implicit def double2Complex(x: Double): Complex = Complex(x, 0)
    
    implicit def int2Complex(x: Int): Complex = Complex(x, 0)
    
    
}
