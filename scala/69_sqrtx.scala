
/**
* select solution
* binary search - recursive
* memo:
*   1. maintain max and min
* time complexity: O(logN)
*/
object Solution0 {
    def mySqrt(x: Int): Int = {
        if(x == 0 || x == 1) return x
        _mySqrt(0, x, x, math.pow(10, -5)).toInt
    }
    
    @annotation.tailrec
    def _mySqrt(min:Double, max: Double, target:Int, precision: Double): Double = {
        val guess = min + (max - min) / 2
        val estimate = guess * guess
        if(math.abs(estimate - target) < precision) guess
        else{ 
            if(estimate > target) _mySqrt(min, guess, target, precision)
            else _mySqrt(guess, max, target, precision)
        } 
    }
}


/**
* my first commitment
* binary search- iterative
* time complexity: O(LogN)
*/
object Solution1 {
  def mySqrt(x: Int): Int = {
    if(x == 0 || x== 1) return x

    val precision = math.pow(10, -5)
    var high: Double = if (x > 1) x else 1
    var low: Double = 0

    while(true) {
      val mid: Double = low + ((high - low) / 2)
      val estimate = mid * mid

      if(math.abs(estimate - x) < precision){
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
* binary search - iterative
* not return while in while block
*/
object Solution1-2 {
    def mySqrt(x: Int): Int = {
        if(x == 0 || x == 1) return x
        val precision = math.pow(10, -5)
        var max: Double = if(x > 1) x.toDouble else 1.0
        var min = 0.0
        var mid = min + (max - min) / 2 
        var condition = true
        
        while(condition){
            mid = min + (max - min) / 2 
            val estimate = mid * mid
            
            if(math.abs(estimate - x) < precision){
                condition = false
            }else if(estimate > x){
              max = mid  
            } else {
              min = mid
            }
        }
        mid.toInt
    }
}


/**
* binary search - recursive - top-down
* memo:
*   1. maintain max and min
*/
object Solution1-3 {
    def mySqrt(x: Int): Int = {
        if(x == 0 || x == 1) return x
        _mySqrt(0, x, x, math.pow(10, -5)).toInt
    }
    
    @annotation.tailrec
    def _mySqrt(min:Double, max: Double, target:Int, precision: Double): Double = {
        val guess = min + (max - min) / 2
        val estimate = guess * guess
        if(math.abs(estimate - target) < precision) guess
        else{
            if(estimate > target) _mySqrt(min, guess, target, precision)
            else _mySqrt(guess, max, target, precision)
        } 
    }
}

/**
* Newton's method - iterative
* y = x^2 => f(x) = x^2 - y
* x_{k+1} = x_k - f(x_k) / f'(x_k)
* x_{k+1} = x_k - (x_k^2 - y) / (2x_k) = (x_k + y / x_k) / 2
* time complexity: O(logN)
*/

object Solution2 {
     def mySqrt(x: Int): Int = {
        val precision = math.pow(10, -5)
        
        var ans: Double = x
        while(math.abs(ans * ans - x) > precision){
            ans = (ans + x / ans) / 2
            // println(ans)
        }
        ans.toInt
    }
}

/**
*  newton-method - recursive - top-down
*/

object Solution {
    def mySqrt(x: Int): Int = {
        _mySqrt(x, x, math.pow(10, -5)).toInt
    }

    @annotation.tailrec
    def _mySqrt(guess: Double, target: Int, precision: Double): Double = {
        /* see? (guess * guess - target) is just our f(x) =  x^2 - y */
        if(math.abs(guess * guess - target) < precision) guess
        else _mySqrt((guess + (target / guess)) / 2, target, precision)
    }
}