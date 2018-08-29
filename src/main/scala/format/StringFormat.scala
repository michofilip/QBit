package format

object StringFormat {
    
    implicit class Fit(str: String) {
        def justRight(length: Int): String = {
            Seq.fill(length - str.length)(" ").mkString + str
        }
        
        def justLeft(length: Int): String = {
            str + Seq.fill(length - str.length)(" ").mkString
        }
    }
    
}
