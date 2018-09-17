package quantum

import scala.language.implicitConversions

class QuantumGateColumn(val quantumGates: IndexedSeq[QuantumGate]) {
    def qbits: Int = quantumGates.foldLeft(0)(_ + _.qbits)
    
    def *(that: QuantumGateColumn): QuantumGateColumn = new QuantumGateColumn(this.quantumGates ++ that.quantumGates)
    
    def invert: QuantumGateColumn = new QuantumGateColumn(quantumGates.reverse.map(_.invert))
}

object QuantumGateColumn {
    implicit def qg2qgc(quantumGate: QuantumGate): QuantumGateColumn = new QuantumGateColumn(IndexedSeq(quantumGate))
    
    //    def apply(
    //
    //    val quantumGates: IndexedSeq[QuantumGate]
    //    ): QuantumGateColumn = new QuantumGateColumn(
    //    val quantumGates
    //    )
    
}
