import quantum.QuantumGate._
import quantum.QuantumState._
import quantum.QuantumGate

object Main extends App {
    
    println(I)
    println()
    println(X)
    println()
    println(Y)
    println()
    println(Z)
    println()
    println(R(Math.PI / 4))
    println()
    println(H)
    println()
    println(SWAP)
    println()
    println(CNOT)
    println()
    println(CCNOT)
    println()
    println(controlled(QuantumGate.H, negated = true, under = true))
    println()
    
    println(zero)
    println(one)
    println(zero * zero)
    println(zero * one)
    println(one * zero)
    println(one * one)
    
    println(SWAP.applyOn(zero * zero))
    println(SWAP.applyOn(zero * one))
    println(SWAP.applyOn(one * zero))
    println(SWAP.applyOn(one * one))
    println(H.applyOn(zero))
    println(H.applyOn(one))
    
}
