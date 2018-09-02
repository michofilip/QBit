package format

object NumberFormat {
    
    implicit class Round(x: Double) {
        def round(decimals: Int): Double = {
            val pow = Math.pow(10, decimals)
            val sign = if (x >= 0) 1 else -1
            sign * Math.round(sign * x * pow) / pow
        }
    }
    
}
