package format

object StringFormat {
    
    implicit class Fit(str: String) {
        def alignRight(length: Int): String = Seq.fill(length - str.length)(" ").mkString + str
        
        def alignLeft(length: Int): String = str + Seq.fill(length - str.length)(" ").mkString
    }
    
}
