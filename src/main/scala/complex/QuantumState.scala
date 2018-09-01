package complex

class QuantumState(private val vector: ComplexVector) {
    val qbits: Int = {
        val log = Math.log(vector.dimension) / Math.log(2)
        val logInt = log.toInt
        require(logInt == log)
        logInt
    }
    require(vector.lengthSqr == 1)
    
    override def toString: String = {
        def toBin(n: Int, size: Int): String = {
            def f(n: Int, size: Int, arr: Vector[Int]): Vector[Int] = {
                if (size > 0) {
                    f(n / 2, size - 1, (n % 2) +: arr)
                } else {
                    arr
                }
            }
            
            f(n, size, Vector.empty).mkString
        }
        
        val coefficients: Seq[Complex] = for (i <- 1 to vector.dimension) yield vector(i)
        val coefficientsZipped = coefficients.zipWithIndex.filterNot {
            case (c, _) => c == Complex(0)
        }
        
        coefficientsZipped.map {
            case (c, i) => "(" + c + ")|" + toBin(i, qbits) + ">"
        }.mkString(" + ")
    }
    
    def *(that: QuantumState): QuantumState = {
        new QuantumState(this.vector tensorProduct that.vector)
    }
}

object QuantumState {
    def apply(coefficients: Complex*): QuantumState = new QuantumState(ComplexVector(coefficients: _*).normalize)
}
