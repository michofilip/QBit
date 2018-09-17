package quantum

import scala.language.implicitConversions

class QuantumSystem1(val qbits: Int, val columns: IndexedSeq[QuantumGateColumn]) {
    require(columns.forall(c => c.qbits == qbits))
    
    def size: Int = columns.length
    
    def >(that: QuantumSystem1): QuantumSystem1 = {
        require(this.qbits == that.qbits)
        new QuantumSystem1(qbits, this.columns ++ that.columns)
    }
    
    //    def *(that:QuantumSystem1):QuantumSystem1
    
    def invert: QuantumSystem1 = new QuantumSystem1(qbits, columns.reverse.map(_.invert))
    
    //    override def toString: String = quantumGates.mkString("\n\n")
}


