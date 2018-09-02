package quantum

import scala.language.implicitConversions

class QuantumSystem(val qbits: Int, val quantumGates: IndexedSeq[QuantumGate]) {
    require(quantumGates.forall(q => q.qbits == qbits))
    
    def >(that: QuantumSystem): QuantumSystem = new QuantumSystem(qbits, this.quantumGates ++ that.quantumGates)
    
    def invert: QuantumSystem = new QuantumSystem(qbits, quantumGates.reverse.map(_.invert))
    
    override def toString: String = quantumGates.mkString("\n\n")
}

object QuantumSystem {
    def apply(qbits: Int): QuantumSystem = new QuantumSystem(qbits, IndexedSeq.empty)
    
    implicit def g2s(quantumGate: QuantumGate): QuantumSystem = new QuantumSystem(quantumGate.qbits, IndexedSeq(quantumGate))
}
