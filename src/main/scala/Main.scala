import quantum.QuantumGate._
import quantum.QuantumState._
import quantum.QuantumSystem._

import scala.util.Random

object Main extends App {
    implicit val rand: Random = new Random()
    
    println("teleportation")
    val state = randomQbit * q0 * q0
    val teleportation = I * H * I > I * CNOT > CNOT * I > H * I * I > I * CNOT > C(I * Z) > M * M * I
    println(state)
    println()
    for (_ <- 1 to 10) {
        val qs = state applySystem teleportation
        println(qs)
    }
    
    println()
    println("superdense coding")
    val state00 = q0 * q0 * q0 * q0
    val state01 = q0 * q1 * q0 * q0
    val state10 = q1 * q0 * q0 * q0
    val state11 = q1 * q1 * q0 * q0
    val superdensecoding = I * I * H * I > I * I * CNOT > I * CNOT * I > C(I * Z) * I > I * I * CNOT > I * I * H * I > I * I * M * M
    println(state00.applySystem(superdensecoding))
    println(state01.applySystem(superdensecoding))
    println(state10.applySystem(superdensecoding))
    println(state11.applySystem(superdensecoding))
    
    //    println()
    //    println((q0 * q0 * q0 * q0 * q0 * q0).applySystem(H * H * H * H * H * H))
    
}
