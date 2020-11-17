/**
* select solution
* recursive - bottom-up
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
* recursive version : bottom-up
* memo
*   1. n may be negative or positive
*   2. n may be odd or even
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
/**
* recursive version : bottom-up
*/
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

/**
* top-down - iterative version 
* Binary Exponentiation with negative n
*
* each iteration is calculate pow(base, nn) * ans
*   ex: input x = 2, n = 10
*    0. base: 2.0, nn: 10 ans: 1.0 => pow(2, 10) * 1 =  1024
*    1. base: 4.0, nn: 5, ans: 1.0  => pow(4, 5) * 1 = 1024
*    2. base: 16.0, nn: 2, ans: 4.0 => pow(16, 2) * 4 = 1024
*    3. base: 256.0, nn: 1, ans: 4.0 => pow(256, 1) * 4 = 1024
*    4. base: 65536.0, nn: 0, ans: 1024.0 => pow(65536, 0) * 1024 = 1024
*
* time complexity: O(logN)
*/

object Solution2 {
  def myPow(x: Double, n: Int): Double = {
    if (n == 0) return 1
    var ans = 1.0
    var nn = n
    var base = x

    while (nn != 0) {
     /* nn could be -1 if nn < 0 and run nn % 2, so using nn & 1 here */
      if((nn & 1) == 1)  ans = ans * base
      nn = nn / 2
      base = base * base
    }
    // judge n to decide whether reverse ans
    if (n < 0) 1.0 / ans else ans  
  }
}

/**
* top-down - recursive version - 
*/
object Solution2-1 {
    def myPow(x: Double, n: Int): Double = {
      val ans = _myPow(1, x, n)
      if(n < 0) 1 / ans else ans
    }
    
    @annotation.tailrec
    def _myPow(current: Double, base: Double, pow: Int): Double = {
        if(pow == 0) current
        else{
            if((pow & 1) == 1) _myPow(current * base, base * base, pow / 2)
            else _myPow(current, base * base, pow / 2)
        }
    }
}