package format

object NumberFormat {
    
    implicit class Trim(x: Double) {
        def trim(decimals: Int): Double = {
            val pow = Math.pow(10, decimals)
            val sign = if (x >= 0) 1 else -1
            sign * Math.round(sign * x * pow) / pow
        }
    }
    
}
