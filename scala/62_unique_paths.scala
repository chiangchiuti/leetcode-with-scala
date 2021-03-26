
/**
* my first commitment: math combination
* memo:
*  1. the total walk steps is (m-1) + (n -1) : (m-1) steps go down and (n-1) steps go right
*  2. unique paths is calculated by C^{m - 1 + n - 1}_{ m - 1} * C^{n-1}_{n-1}
*/
object Solution1 {
    def uniquePaths(m: Int, n: Int): Int = {
      val allStep = (m - 1)  +  (n - 1)
      calCombination(allStep, (m - 1))     
    }
    def calCombination(a: Int, b: Int): Int = {
      val c = a - b
      val max = c max b
      val min = c min b
      val numerator = (BigInt(a) until max by -1).product
      val denominator = (BigInt(min) to 1 by -1).product
      (numerator / denominator).toInt
    }
}

/**
* long type
*/
object Solution1-2 {
    def uniquePaths(m: Int, n: Int): Int = {
      val allStep = (m - 1)  + (n - 1)
      calCombination(allStep.toLong, (m - 1).toLong).toInt 
    }
    def calCombination(a: Long, b: Long): Long = {
      val c = a - b
      val max = c max b
      val min = c min b
      val numerator = (a until max by -1).product
      val denominator = (min to 1 by -1).product
      (numerator / denominator)
    }
}

/**
* dynamic programming
* time complexity: O(N *M)
* space complexity: O(N * M)
*/
object Solution2 {
    def uniquePaths(m: Int, n: Int): Int = {
      val dp = Array.tabulate[Int](m, n) {
        case (0, j) => 1
        case (i, 0) => 1
        case _ => 0
      }
      for (i <- 1 until m; j <- 1 until n) {
        dp(i)(j) = dp(i - 1)(j) + dp(i)(j - 1)
      }
      
      dp.last.last
    }
}

/**
* fill dp array with 1
*/
object Solution2-1{
    def uniquePaths(m: Int, n: Int): Int = {
      val dp = Array.fill[Int](m, n)(1)
      for (i <- 1 until m; j <- 1 until n) {
        dp(i)(j) = dp(i - 1)(j) + dp(i)(j - 1)
      }
      
      dp.last.last
    }
}