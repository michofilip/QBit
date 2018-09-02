package quantum

import complex.Complex._
import complex.{Complex, ComplexMatrix}

class QuantumGate(val matrix: ComplexMatrix, val measured: Set[Int]) {
    val qbits: Int = {
        val log = Math.log(matrix.dimension) / Math.log(2)
        val logInt = log.toInt
        require(logInt == log)
        logInt
    }
    require(this.matrix * this.matrix.transpose.conjugate == ComplexMatrix.id(1 << qbits))
    
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
    def I: QuantumGate = {
        val matrix = ComplexMatrix.id(2)
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
    
    def C(quantumGate: QuantumGate): QuantumGate = controlled(quantumGate)
    
    def nC(quantumGate: QuantumGate): QuantumGate = controlled(quantumGate, negated = true)
    
    def uC(quantumGate: QuantumGate): QuantumGate = controlled(quantumGate, under = true)
    
    def unC(quantumGate: QuantumGate): QuantumGate = controlled(quantumGate, negated = true, under = true)
    
    def controlled(quantumGate: QuantumGate, negated: Boolean = false, under: Boolean = false): QuantumGate = {
        import ComplexMatrix.id
        val dim = quantumGate.matrix.dimension
        
        val qbits = quantumGate.qbits + 1
        lazy val neg: ComplexMatrix = ComplexMatrix(0, 1, 1, 0) tensorProduct id(dim)
        lazy val shift = (for (q <- 0 to qbits - 2) yield
            id(1 << q) tensorProduct SWAP.matrix tensorProduct id(1 << (qbits - 2 - q)))
                .reduce((acc, m) => m * acc)
        val baseMatrix = ComplexMatrix(
            (for (i <- 1 to 2 * dim; j <- 1 to 2 * dim) yield
                if (i > dim && j > dim) quantumGate.matrix(i - dim, j - dim)
                else if (i == j) Complex(1)
                else Complex(0)): _*)
        
        val matrix = if (negated && under) {
            shift.transpose.conjugate * neg.transpose.conjugate * baseMatrix * neg * shift
        } else if (negated && !under) {
            neg.transpose.conjugate * baseMatrix * neg
        } else if (!negated && under) {
            shift.transpose.conjugate * baseMatrix * shift
        } else {
            baseMatrix
        }
        
        new QuantumGate(matrix, Set.empty)
    }
    
    def M: QuantumGate = {
        val matrix = ComplexMatrix(
            1, 0,
            0, 1
        )
        new QuantumGate(matrix, Set(1))
    }
}