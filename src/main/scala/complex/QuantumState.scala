package complex

class QuantumState(complexVector: ComplexVector) {
    val qbits: Int = {
        val log = Math.log(complexVector.dimension) / Math.log(2)
        val logInt = log.toInt
        require(logInt == log)
        logInt
    }
    
    
//    override def toString: String = {
//        val coefficients: Seq[Complex] = for (i <- 1 to complexVector.dimension) yield complexVector(i)
//
//        coefficients.filterNot(c => c == Complex(0)).mkString("(", "", ")")
//    }
}

object QuantumState {
    def apply(coefficients: Complex*): QuantumState = new QuantumState(ComplexVector(coefficients: _*))
    
}
