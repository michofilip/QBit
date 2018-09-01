package complex

import format.StringFormat.Fit

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
    
    def *(that: ComplexMatrix): ComplexMatrix = {
        require(this.dimension == that.dimension)
        
        val coefficients = for (i <- 1 to dimension; j <- 1 to dimension) yield
            (for (t <- 1 to dimension) yield apply(i, t) * that.apply(t, j)).foldLeft(Complex(0))(_ + _)
        
        ComplexMatrix(coefficients: _*)
    }
    
    def *(complexVector: ComplexVector): ComplexVector = {
        require(dimension == complexVector.dimension)
        
        val coefficients = for (i <- 1 to dimension)
            yield (for (j <- 1 to dimension) yield apply(i, j) * complexVector(j)).foldLeft(Complex(0))(_ + _)
        
        ComplexVector(coefficients: _*)
    }
    
    def conjugate: ComplexMatrix = new ComplexMatrix(dimension, coefficientMap.mapValues(c => c.conjugate))
    
    def transpose: ComplexMatrix = new ComplexMatrix(dimension, coefficientMap.map {
        case ((i, j), c) => ((j, i), c)
    })
    
    def tensorProduct(that: ComplexMatrix): ComplexMatrix = {
        ComplexMatrix((for (i1 <- 1 to dimension; i2 <- 1 to that.dimension; j1 <- 1 to dimension; j2 <- 1 to that.dimension)
            yield apply(i1, j1) * that.apply(i2, j2)): _*)
    }
    
    override def toString: String = {
        val columns: IndexedSeq[IndexedSeq[Complex]] = for (j <- 1 to dimension) yield for (i <- 1 to dimension) yield apply(i, j)
        val maxLengths: IndexedSeq[Int] = columns.map(column => column.map(c => c.toString.length).max)
        val columnsStr: IndexedSeq[IndexedSeq[String]] = columns.zip(maxLengths).map {
            case (column, max) => column.map(c => c.toString.justRight(max))
        }
        
        (for (i <- 1 to dimension) yield (for (j <- 1 to dimension) yield columnsStr(j - 1)(i - 1)).mkString("|", ", ", "|")).mkString("\n")
    }
    
    override def hashCode(): Int = coefficientMap.hashCode()
    
    override def equals(obj: scala.Any): Boolean = obj match {
        case m: ComplexMatrix => coefficientMap == m.coefficientMap
        case _ => false
    }
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
    
    def pow(matrix: ComplexMatrix, n: Int): ComplexMatrix = {
        require(n >= 0)
        
        def p(m: ComplexMatrix, n: Int): ComplexMatrix = if (n > 0) p(m * matrix, n - 1) else m
        
        p(id(matrix.dimension), n)
    }
}
