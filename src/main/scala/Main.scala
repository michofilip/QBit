import quantum.QuantumGate._
import quantum.QuantumMachine
import quantum.QuantumState._

import scala.util.Random

object Main extends App {
    implicit val rand: Random = new Random()
    
    println(QuantumMachine(qbit0 * qbit0).applyAll(H * I, CNOT))
    println(QuantumMachine(qbit0 * qbit0).applyAll(H * I, CNOT, M * I))
    println(QuantumMachine(qbit0 * qbit0).applyAll(H * I, CNOT, I * M))
    
    println(QuantumMachine(qbit0).applyAll(M))
    println(QuantumMachine(qbit1).applyAll(M))
    //    println(QuantumMachine(qbit(.1)).applyOne(X))
    //    println(QuantumMachine(qbit(.9)))
    //    println(QuantumMachine(qbit(.9)).applyOne(X))
    for (_ <- 1 to 100) {
        //        println(QuantumMachine(qbit(.9)).applyAll(M))
    }
    println((M * I * M).measured)
    val amp = 0.9
    println(Math.sqrt(1 - amp * amp))
}
