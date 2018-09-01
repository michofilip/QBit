import complex.Complex._
import complex.{ComplexMatrix, ComplexVector, QuantumGate, QuantumState}
import format.NumberFormat.Trim
import format.StringFormat.Fit

object Main extends App {
    for (y <- -1 to 1; x <- -1 to 1) {
        println(x + y * i)
    }
    
    println("*******")
    
    val v1 = ComplexVector(1, -1)
    val v2 = ComplexVector(2, 2)
    println(v1)
    println(v2)
    println(v1 tensorProduct v2)
    
    val c1 = 1 + 2 * i
    val c2 = 1 + 2 * i
    println(c1 == c2)
    
    println(ComplexVector(1, 0) dot ComplexVector(0, 1))
    println(v1.conjugate / 2)
    
    val m1 = ComplexMatrix(
        1, 4 + i,
        3, -1
    )
    
    val m2 = ComplexMatrix(
        2, 0,
        1, -3
    )
    
    println(m1)
    println()
    println(m2)
    println()
    println(m1 * m2)
    
    val x = -10.36452837568732
    println(x)
    println(x.trim(3))
    
    println("abc".justRight(5))
    println("abc".justLeft(5) + "|")
    
    
    val I = ComplexMatrix.id(2)
    val H = ComplexMatrix(
        1, 1,
        1, -1
    ) / Math.sqrt(2)
    println(I)
    println(H)
    println(H.conjugate.transpose)
    println(H == H.conjugate.transpose)
    
    println()
    println(I tensorProduct I)
    println()
    println(I tensorProduct H)
    println()
    println(H tensorProduct I)
    println()
    println(H tensorProduct H)
    
    
    println(m2 * ComplexVector(1, 2))
    
    val qs = QuantumState(1, 1, 1, 1)
    println(qs)
    
    println()
    println(QuantumGate.I)
    println()
    println(QuantumGate.X)
    println()
    println(QuantumGate.Y)
    println()
    println(QuantumGate.Z)
    println()
    println(QuantumGate.R(Math.PI / 4))
    println()
    println(QuantumGate.H)
    println()
    println(QuantumGate.SWAP)
    println()
    println(QuantumGate.CNOT)
    println()
    println(QuantumGate.CCNOT)
    println()
    println(QuantumGate.controlled(QuantumGate.SWAP, true, true))
    println()
    
    
}
