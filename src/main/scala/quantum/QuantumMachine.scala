//package quantum
//
//import complex.Complex
//
//import scala.util.Random
//
//class QuantumMachine(val quantumState: QuantumState) {
//    def qbits: Int = quantumState.qbits
//
//    def applySystem(quantumSystem: QuantumSystem)(implicit random: Random): QuantumMachine = {
//        quantumSystem.quantumGates.foldLeft(this)((qm, qg) => {
//            require(qbits == qg.qbits)
//
//            val qs = QuantumState(qg.matrix * qm.quantumState.vector)
//            val qsm = qg.measured.foldLeft(qs)(measure)
//
//            new QuantumMachine(qsm)
//        })
//    }
//
//    private def measure(quantumState: QuantumState, qbitNo: Int)(implicit random: Random): QuantumState = {
//        require(1 <= qbitNo && qbitNo <= qbits)
//
//        val vector = quantumState.vector
//        val dim = vector.dimension
//        val rand = random.nextDouble()
//
//        val selector1: Set[Int] = (1 to dim).filter(i => ((i - 1) >> (qbits - qbitNo)) % 2 == 1).toSet
//        val prob1 = selector1.foldLeft(0.0)((p, i) => p + vector(i).absSqr)
//
//        val coeff = if (rand < prob1) {
//            for (i <- 1 to dim) yield if (!selector1(i)) Complex(0) else vector(i)
//        } else {
//            for (i <- 1 to dim) yield if (selector1(i)) Complex(0) else vector(i)
//        }
//
//        QuantumState(coeff: _*)
//    }
//
//    override def toString: String = quantumState.toString
//}
//
//object QuantumMachine {
//    def apply(quantumState: QuantumState): QuantumMachine = new QuantumMachine(quantumState)
//}
