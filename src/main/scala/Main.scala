import quantum.QuantumGate._
import quantum.QuantumState._
import quantum.QuantumSystem._

import scala.util.Random

object Main extends App {
    implicit val rand: Random = new Random()
    
    val state = qbit(.5) * qbit0 * qbit0
    val teleportation = I * H * I > I * CNOT > CNOT * I > H * I * I > I * CNOT > C(I * Z) > M * M * I
    println(state)
    println()
    for (_ <- 1 to 10) {
        val qs = state applySystem teleportation
        println(qs)
    }
    
    val state1 = qbit0 * qbit1 * qbit0 * qbit0
    val superdensecoding = I * I * H * I > I * I * CNOT > I * CNOT * I > C(I * Z) * I > I * I * CNOT > I * I * H * I > I * I * M * M
    
    println(state1.applySystem(superdensecoding))
}
