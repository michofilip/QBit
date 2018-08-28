package complex

class ComplexVector private(val dimension: Int, _coefficients: Map[Int, Complex]) {
    private val coefficients: Map[Int, Complex] = _coefficients.filterNot {
        case (_, c) => c == Complex(0)
    }
    
    //    def get(index: Int): Option[Complex] = coefficients.get(index)
    
    def apply(index: Int): Complex = {
        require(1 <= index && index <= dimension)
        coefficients.getOrElse(index, Complex(0))
    }
    
    def unary_+ : ComplexVector = new ComplexVector(dimension, coefficients)
    
    def unary_- : ComplexVector = new ComplexVector(dimension, coefficients.mapValues(c => -c))
    
    def +(that: ComplexVector): ComplexVector = {
        require(this.dimension == that.dimension)
        ComplexVector((for (i <- 1 to dimension) yield apply(i) + that.apply(i)): _*)
    }
    
    def -(that: ComplexVector): ComplexVector = this + -that
    
    def *(complex: Complex): ComplexVector = new ComplexVector(dimension, coefficients.mapValues(c => c * complex))
    
    def dot(that: ComplexVector): Complex = {
        require(this.dimension == that.dimension)
        (for (i <- 1 to dimension) yield apply(i) * that.apply(i)).foldLeft(Complex(0))(_ + _)
    }
    
    override def toString: String = (for (i <- 1 to dimension) yield apply(i)).mkString("(", ", ", ")")
}

object ComplexVector {
    
    implicit class CV(x: Double) {
        def *(complexVector: ComplexVector): ComplexVector = complexVector * x
    }
    
    def apply(complexSeq: Complex*): ComplexVector = {
        val dimension = complexSeq.length
        val coefficients = (1 to dimension).zip(complexSeq).toMap
        new ComplexVector(dimension, coefficients)
    }
    
}
