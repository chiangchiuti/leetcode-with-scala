/**
* chosen solution
* iterative - bottom up with memorization
* only record n -1 and n -2 status
* time complexity O(N)
* space complexity O(1)
*/ 
object Solution0 {
    def fib(N: Int): Int = {
        if (N <= 1) N
        else {
            var a = 0
            var b = 1
            (1 until N).foreach{ n =>
                val c = a + b
                a = b
                b = c
            }
            b
        }
    }
}

/**
* iterative - bottom up with memorization
* time complexity O(N)
* space complexity O(N)
*/

object Solution1 {
    def fib(N: Int): Int = {
        if(N <= 1) return N
        
        val cache = Array.ofDim[Int](N + 1)
        cache(0) = 0
        cache(1) = 1
        (2 to N).foreach(n => cache(n) = cache(n -1) + cache(n -2))
        cache(N)
    }
}

/**
* iterative - bottom up with memorization
* only record n -1 and n -2 status
* time complexity O(N)
* space complexity O(1)
*/ 
object Solution1-2 {
    def fib(N: Int): Int = {
        if (N <= 1) N
        else {
            var a = 0
            var b = 1
            (1 until N).foreach{ n =>
                val c = a + b
                a = b
                b = c
            }
            b
        }
    }
}

/**
* recursive - top-down with memorization
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution2 {
    def fib(N: Int): Int = {
        if(N <= 1) return N
        
        val cache = Array.ofDim[Int](N + 1)
        cache(0) = 0
        cache(1) = 1

        def _fib(n: Int):Int = {
            if(n <= 1) return n
            
            if(cache(n) != 0) cache(n)
            else {
                cache(n) = _fib(n-1) + _fib(n-2)
                cache(n)
            }
        }
        _fib(N)
        
    }
}



/**
* recursive version - bottom up
* time complexity: O(2^N）
* space complexity: O(N)
*/

object Solution3 {
    def fib(N: Int): Int = {
        if (N <= 1) N
        else {
           fib(N - 1) + fib(N - 2)
        }
    }
}


/**
* matrix operation with pow operation
* memo
*   n > 1
*   | fn   |    | 1  1  |^ (n -1)  | 1 |
*   | fn-1 |  = | 1  0  |          | 1 |
* time complexity: O(logN)
* space complexity: O(logN) due to stack size
*/
object Solution4 {
  def fib(N: Int): Int = {
    if (N <= 1) return N

    val matrix = Array.ofDim[Int](2, 2)
    matrix(0)(0) = 1
    matrix(0)(1) = 1
    matrix(1)(0) = 1
    matrix(1)(1) = 0

    val identityMatrix = Array.tabulate(2, 2) {
      case (i, j) if i == j => 1
      case _ => 0
    }
    val retMatrix = matrixPow(identityMatrix, matrix, N - 1)
    retMatrix(0)(0)
  }

  @annotation.tailrec
  def matrixPow(current: Array[Array[Int]], base: Array[Array[Int]], pow: Int): Array[Array[Int]] = {
    if (pow == 0) current
    else {
      if ((pow & 1) == 1) {
        val newCurrent = multiply(current, base)
        val newBase = multiply(base, base)

        matrixPow(newCurrent, newBase, pow / 2)
      } else {
        val newBase = multiply(base, base)
        matrixPow(current, newBase, pow / 2)
      }
    }

  }

  def multiply(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val a00 = a(0)(0) * b(0)(0) + a(0)(1) * b(1)(0)
    val a01 = a(0)(0) * b(0)(1) + a(0)(1) * b(1)(1)
    val a10 = a(1)(0) * b(0)(0) + a(1)(1) * b(1)(0)
    val a11 = a(1)(0) * b(0)(1) + a(1)(1) * b(1)(1)
    a(0)(0) = a00
    a(0)(1) = a01
    a(1)(0) = a10
    a(1)(1) = a11
    a
  }
}