package complex

class ComplexVector private(val dimension: Int, _coefficientMap: Map[Int, Complex]) {
    private val coefficientMap: Map[Int, Complex] = _coefficientMap.filterNot {
        case (_, c) => c == Complex(0)
    }
    
    def apply(i: Int): Complex = {
        require(1 <= i && i <= dimension)
        coefficientMap.getOrElse(i, Complex(0))
    }
    
    def unary_+ : ComplexVector = this
    
    def unary_- : ComplexVector = new ComplexVector(dimension, coefficientMap.mapValues(c => -c))
    
    def +(that: ComplexVector): ComplexVector = {
        require(this.dimension == that.dimension)
        ComplexVector((for (i <- 1 to dimension) yield this.apply(i) + that.apply(i)): _*)
    }
    
    def -(that: ComplexVector): ComplexVector = this + -that
    
    def *(complex: Complex): ComplexVector = new ComplexVector(dimension, coefficientMap.mapValues(c => c * complex))
    
    def /(complex: Complex): ComplexVector = this * (1 / complex)
    
    def dot(that: ComplexVector): Complex = {
        require(this.dimension == that.dimension)
        (for (i <- 1 to dimension) yield this.apply(i) * that.apply(i)).reduceLeft(_ + _)
    }
    
    def tensorProduct(that: ComplexVector): ComplexVector = {
        ComplexVector((for (i1 <- 1 to this.dimension; i2 <- 1 to that.dimension) yield this.apply(i1) * that.apply(i2)): _*)
    }
    
    def conjugate: ComplexVector = new ComplexVector(dimension, coefficientMap.mapValues(c => c.conjugate))
    
    def lengthSqr: Double = (for (i <- 1 to dimension) yield apply(i).absSqr).sum
    
    def length: Double = Math.sqrt(lengthSqr)
    
    def normalize: ComplexVector = this / length
    
    override def toString: String = (for (i <- 1 to dimension) yield apply(i)).mkString("(", ", ", ")")
}

object ComplexVector {
    
    implicit class CV(x: Double) {
        def *(complexVector: ComplexVector): ComplexVector = complexVector * x
    }
    
    def apply(coefficients: Complex*): ComplexVector = {
        val dimension = coefficients.length
        val coefficientMap = (1 to dimension).zip(coefficients).toMap
        new ComplexVector(dimension, coefficientMap)
    }
    
}
