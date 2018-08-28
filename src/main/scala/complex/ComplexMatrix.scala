package complex

class ComplexMatrix(val dimension: Int, _coefficientMap: Map[(Int, Int), Complex]) {
    private val coefficientMap: Map[(Int, Int), Complex] = _coefficientMap.filterNot {
        case (_, c) => c == Complex(0)
    }
    
    def apply(i: Int, j: Int): Complex = {
        require(1 <= i && i <= dimension && 1 <= j && j <= dimension)
        coefficientMap.getOrElse((i, j), Complex(0))
    }
    
    def unary_+ : ComplexMatrix = this
    
    def unary_- : ComplexMatrix = new ComplexMatrix(dimension, coefficientMap.mapValues(c => -c))
    
    def +(that: ComplexMatrix): ComplexMatrix = {
        require(this.dimension == that.dimension)
        ComplexMatrix((for (i <- 1 to dimension; j <- 1 to dimension) yield apply(i, j) + that.apply(i, j)): _*)
    }
    
    def -(that: ComplexMatrix): ComplexMatrix = this + -that
    
    def *(complex: Complex): ComplexMatrix = new ComplexMatrix(dimension, coefficientMap.mapValues(c => c * complex))
    
    def /(complex: Complex): ComplexMatrix = this * (1 / complex)
    
    def *(complex: ComplexMatrix): ComplexMatrix = {
        
        
        ???
    }
    
    def conjugate: ComplexMatrix = new ComplexMatrix(dimension, coefficientMap.mapValues(c => c.conjugate))
    
}

object ComplexMatrix {
    
    implicit class CV(x: Double) {
        def *(complexMatrix: ComplexMatrix): ComplexMatrix = complexMatrix * x
    }
    
    def apply(coefficients: Complex*): ComplexMatrix = {
        val dimension: Int = {
            val dim = Math.sqrt(coefficients.length)
            val dimInt = dim.toInt
            require(dim == dimInt)
            dimInt
        }
        val coefficientMap = (for (i <- 1 to dimension; j <- 1 to dimension) yield (i, j)).zip(coefficients).toMap
        new ComplexMatrix(dimension, coefficientMap)
    }
    
    def diagonal(coefficients: Complex*): ComplexMatrix = {
        val dimension = coefficients.length
        val coefficientMap = (for (i <- 1 to dimension) yield (i, i)).zip(coefficients).toMap
        new ComplexMatrix(dimension, coefficientMap)
    }
    
    def id(dimension: Int): ComplexMatrix = diagonal(Seq.fill(dimension)(Complex(1)): _*)
    
}
