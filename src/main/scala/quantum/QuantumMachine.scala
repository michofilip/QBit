package quantum

import complex.Complex

import scala.util.Random

class QuantumMachine(val quantumState: QuantumState) {
    def qbits: Int = quantumState.qbits
    
    def applyOne(quantumGate: QuantumGate)(implicit random: Random): QuantumMachine = {
        require(qbits == quantumGate.qbits)
        
        val qs = QuantumState(quantumGate.matrix * quantumState.vector)
        val qsm = quantumGate.measured.foldLeft(qs)(measure)
        
        new QuantumMachine(qsm)
    }
    
    def applyAll(quantumGates: QuantumGate*)(implicit random: Random): QuantumMachine = {
        quantumGates.foldLeft(this)(_.applyOne(_))
    }
    
    private def measure(quantumState: QuantumState, qbitNo: Int)(implicit random: Random): QuantumState = {
        require(1 <= qbitNo && qbitNo <= qbits)
        
        val vector = quantumState.vector
        val dim = vector.dimension
        val rand = random.nextDouble()
        
        val selector1: Set[Int] = (1 to dim).filter(i => ((i - 1) >> (qbits - qbitNo)) % 2 == 1).toSet
        val prob1 = selector1.foldLeft(0.0)(_ + vector(_).absSqr)
        
        val coeff = if (rand < prob1) {
            for (i <- 1 to dim) yield if (!selector1(i)) Complex(0) else vector(i)
        } else {
            for (i <- 1 to dim) yield if (selector1(i)) Complex(0) else vector(i)
        }
        
        QuantumState(coeff: _*)
    }
    
    override def toString: String = quantumState.toString
    
    
}

object QuantumMachine {
    def apply(quantumState: QuantumState): QuantumMachine = new QuantumMachine(quantumState)
}
