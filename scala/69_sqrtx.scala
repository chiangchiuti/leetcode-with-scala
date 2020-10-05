
/**
* binary search
*/
object Solution {
  def mySqrt(x: Int): Int = {
    if(x == 0 || x== 1) return x

    val error = math.pow(10, -5)
    var high: Double = if (x > 1) x else 1
    var low: Double = 0

    while(true) {
      val mid: Double = low + ((high - low) / 2)
      val estimate = mid * mid

      if(math.abs(estimate - x) < error){
        return mid.toInt

      }else if(estimate > x) {
        high = mid
      }else {
        low = mid
      }
    }
    x
  }
}

/**
* Newton's method
* x_{k+1} = x_k - f(x_k) / f'(x_k)
* x_{k+1} = x_k - (x_k^2 - a) / (2x_k) = (x_k + a / x_k) / 2
*/

object Solution {
    def mySqrt(x: Int): Int = {
        var result: Double = x
        val error = math.pow(10, -5)
        
        while(math.abs(result * result - x) > error) {
            // println(result)
            result = (result + x / result) / 2
        }
        result.toInt
        
    }
}