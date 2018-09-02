package quantum

import complex.{Complex, ComplexVector}

import scala.util.Random

class QuantumState(val vector: ComplexVector) {
    val qbits: Int = {
        val log = Math.log(vector.dimension) / Math.log(2)
        val logInt = log.toInt
        require(logInt == log)
        logInt
    }
    
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
    
    def applySystem(quantumSystem: QuantumSystem)(implicit random: Random): QuantumState = {
        quantumSystem.quantumGates.foldLeft(this)((qs, qg) => {
            require(qbits == qg.qbits)
            
            val qs1 = QuantumState(qg.matrix * qs.vector)
            val qsm = qg.measured.foldLeft(qs1)(measure)
            
            qsm
        })
    }
    
    def measure(quantumState: QuantumState, qbitNo: Int)(implicit random: Random): QuantumState = {
        require(1 <= qbitNo && qbitNo <= qbits)
        
        val vector = quantumState.vector
        val dim = vector.dimension
        val rand = random.nextDouble()
        
        val selector1: Set[Int] = (1 to dim).filter(i => ((i - 1) >> (qbits - qbitNo)) % 2 == 1).toSet
        val prob1 = selector1.foldLeft(0.0)((p, i) => p + vector(i).absSqr)
        
        val coeff = if (rand < prob1) {
            for (i <- 1 to dim) yield if (!selector1(i)) Complex(0) else vector(i)
        } else {
            for (i <- 1 to dim) yield if (selector1(i)) Complex(0) else vector(i)
        }
        
        QuantumState(coeff: _*)
    }
}

object QuantumState {
    def apply(vector: ComplexVector): QuantumState = new QuantumState(vector.normalize)
    
    def apply(coefficients: Complex*): QuantumState = QuantumState(ComplexVector(coefficients: _*))
    
    def qbit(amp1: Double, phase0: Double = 0, phase1: Double = 0): QuantumState = {
        require(0 <= amp1 && amp1 <= 1)
        val amp0 = Math.sqrt(1 - amp1 * amp1)
        QuantumState(amp0 * Complex.expI(phase0), amp1 * Complex.expI(phase1))
    }
    
    def qbit0: QuantumState = qbit(0)
    
    def qbit1: QuantumState = qbit(1)
    
    def phi_+ : QuantumState = QuantumState(ComplexVector(1, 0, 0, 1) / Math.sqrt(2))
    
    def phi_- : QuantumState = QuantumState(ComplexVector(1, 0, 0, -1) / Math.sqrt(2))
    
    def psi_+ : QuantumState = QuantumState(ComplexVector(0, 1, 1, 0) / Math.sqrt(2))
    
    def psi_- : QuantumState = QuantumState(ComplexVector(0, 1, -1, 0) / Math.sqrt(2))
}
