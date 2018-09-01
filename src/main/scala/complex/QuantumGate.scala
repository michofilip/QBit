package complex

import complex.Complex._

class QuantumGate(private val matrix: ComplexMatrix, private val measured: Set[Int]) {
    val qbits: Int = {
        val log = Math.log(matrix.dimension) / Math.log(2)
        val logInt = log.toInt
        require(logInt == log)
        logInt
    }
    require(this.matrix * this.matrix.transpose.conjugate == ComplexMatrix.id(Math.pow(2, qbits).toInt))
    
    def invert: QuantumGate = new QuantumGate(matrix.transpose.conjugate, measured)
    
    override def toString: String = matrix.toString
    
    def *(that: QuantumGate): QuantumGate = {
        new QuantumGate(
            this.matrix tensorProduct that.matrix,
            this.measured ++ that.measured.map(q => q + qbits)
        )
    }
}

object QuantumGate {
    def I: QuantumGate = I(1)
    
    def I(qbits: Int): QuantumGate = {
        val matrix = ComplexMatrix.id(Math.pow(2, qbits).toInt)
        new QuantumGate(matrix, Set.empty)
    }
    
    def X: QuantumGate = {
        val matrix = ComplexMatrix(
            0, 1,
            1, 0
        )
        new QuantumGate(matrix, Set.empty)
    }
    
    def Y: QuantumGate = {
        val matrix = ComplexMatrix(
            0, -i,
            i, 0
        )
        new QuantumGate(matrix, Set.empty)
    }
    
    def Z: QuantumGate = {
        val matrix = ComplexMatrix(
            1, 0,
            0, -1
        )
        new QuantumGate(matrix, Set.empty)
    }
    
    def NOT: QuantumGate = X
    
    def sqrtNOT: QuantumGate = {
        val matrix = ComplexMatrix(
            1 + i, 1 - i,
            1 - i, 1 + i
        ) / 2
        new QuantumGate(matrix, Set.empty)
    }
    
    def H: QuantumGate = {
        val matrix = ComplexMatrix(
            1, 1,
            1, -1
        ) / Math.sqrt(2)
        new QuantumGate(matrix, Set.empty)
    }
    
    def R(phaseShift: Double): QuantumGate = {
        val matrix = ComplexMatrix(
            1, 0,
            0, expI(phaseShift)
        )
        new QuantumGate(matrix, Set.empty)
    }
    
    def SWAP: QuantumGate = {
        val matrix = ComplexMatrix(
            1, 0, 0, 0,
            0, 0, 1, 0,
            0, 1, 0, 0,
            0, 0, 0, 1
        )
        new QuantumGate(matrix, Set.empty)
    }
    
    def sqrtSWAP: QuantumGate = {
        val matrix = ComplexMatrix(
            1, 0, 0, 0,
            0, (1 + i) / 2, (1 - i) / 2, 0,
            0, (1 - i) / 2, (1 + i) / 2, 0,
            0, 0, 0, 1
        )
        new QuantumGate(matrix, Set.empty)
    }
    
    def CNOT: QuantumGate = controlled(NOT)
    
    def CCNOT: QuantumGate = controlled(CNOT)
    
    def CSWAP: QuantumGate = controlled(SWAP)
    
    def controlled(quantumGate: QuantumGate, negated: Boolean = false, down: Boolean = false): QuantumGate = {
        import ComplexMatrix.id
        val dim = quantumGate.matrix.dimension
        
        val qbits = quantumGate.qbits + 1
        lazy val neg: ComplexMatrix = ComplexMatrix(0, 1, 1, 0) tensorProduct id(dim)
        lazy val shift =
            (for (q <- 0 to qbits - 2) yield
                id(Math.pow(2, q).toInt) tensorProduct SWAP.matrix tensorProduct id(Math.pow(2, qbits - 2 - q).toInt)).foldLeft(I(qbits).matrix) {
                case (acc, m) => m * acc
            }
        
        val baseMatrix = ComplexMatrix(
            (for (i <- 1 to 2 * dim; j <- 1 to 2 * dim) yield
                if (i > dim && j > dim) quantumGate.matrix(i - dim, j - dim)
                else if (i == j) Complex(1)
                else Complex(0)
                    ): _*)
        
        val matrix = if (negated && down) {
            shift.transpose.conjugate * neg.transpose.conjugate * baseMatrix * neg * shift
        } else if (negated && !down) {
            neg.transpose.conjugate * baseMatrix * neg
        } else if (!negated && down) {
            shift.transpose.conjugate * baseMatrix * shift
        } else {
            baseMatrix
        }
        new QuantumGate(matrix, Set.empty)
    }
    
    def M: QuantumGate = {
        val matrix = ComplexMatrix(
            0, 1,
            1, 0
        )
        new QuantumGate(matrix, Set(1))
    }
    
//    def repeat(quantumGate: QuantumGate, n: Int): QuantumGate = {
//        require(n >= 0)
//
//        def p(q: QuantumGate, n: Int): QuantumGate = if (n > 0) repeat(q * quantumGate, n - 1) else q
//
//        p(new QuantumGate(ComplexMatrix(1), Set.empty), n)
//    }
}