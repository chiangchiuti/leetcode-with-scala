/**
* select solution
* memo
*   1. n may be negative or positive
*   2. n may be odd or even
* time complexity: O(logN)
*/

object Solution0{
    def myPow(x: Double, n: Int): Double = {
        if(n == 0) return 1
        
        val t = myPow(x, n / 2)
        
        if(n % 2  == 0){
            t * t
        }else{
            if(n < 0) t * t * (1 / x)
            else t * t * x
        }
    }
}

/**
* recursive version 
* O(logN) in time
*/
object Solution1 {
  def myPow(x: Double, n: Int): Double = {
    if (n == 0) 1
    else if(n > 0) {
      n % 2 match{
        case 1 => myPow(x * x, n / 2) * x
        case 0 => myPow(x * x, n / 2)
      }
    }else{
      val t = myPow(x, n / 2)
      math.abs(n % 2) match{
        case 1 => t * t * (1 / x)
        case 0 => t * t
      }
    }

  }
}

object Solution1-2 {
    def myPow(x: Double, n: Int): Double = {
        if(n == 0) return 1
        
        val t = myPow(x, n / 2)
        
        if(n % 2  == 0){
            t * t
        }else{
            if(n < 0) t * t * (1 / x)
            else t * t * x
        }
    }
}