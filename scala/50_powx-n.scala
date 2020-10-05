/**
*  recursive version 
* O(logN) in time
*/
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