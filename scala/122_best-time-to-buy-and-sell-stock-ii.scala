/**
* my first commitment
* greedy alg
* time complexity: O(N)
*/
object Solution1 {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices.length > 1){
            prices.sliding(2).collect{ case arr: Array[Int] if arr(1) > arr(0)=> arr}
    .foldLeft(0){(sum, arr) => 
      sum + arr(1) - arr(0)}
        } else {
            0
        }
    }
}

/**
* greedy alg: one line pass
*/

object Solution1-2 {
  def maxProfit(prices: Array[Int]): Int = {
    if(prices.length > 1) prices.sliding(2).collect{case arr if arr(1) > arr(0) => arr(1) - arr(0)}.sum else 0
  }
}

/**
* dynamic programming 
* time complexity: O(N)
* space complexity: O(2N) create a two-dimension array
*/

object Solution2 {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices == null || prices.isEmpty) return 0   
        /* 
        * state definition: 
        *    0  without holding,
        *    1  holding a share
        */
        val profits = Array.ofDim[Int](prices.length, 2)
        
        profits(0)(0) = 0
        profits(0)(1) = -prices(0)
        for(i <- 1 until prices.length) {
            profits(i)(0) = profits(i - 1)(0) max (profits(i - 1)(1) + prices(i)) //  sell 
            profits(i)(1) = profits(i - 1)(1) max (profits(i - 1)(0) - prices(i)) // buy and hold
        }
        profits.last.max
    }
}
/**
* dynamic programming : simplify above solution
* time complexity: O(N)
* space complexity: O(1)
*/

object Solution2-1 {
    def maxProfit(prices: Array[Int]): Int = {
        if(prices == null || prices.isEmpty) return 0
        val dp = Array.ofDim[Int](2)
        /* 
        * state definition: 
        *    0  without holding,
        *    1  holding a share
        */
        dp(0) = 0
        dp(1) = -prices(0)
        for(i <- 1 until prices.size) {
        /*
        * it may causes a problem here, because we overwrite the previous dp(0) by new state i value and dp(1) would utilizes dp(0) which was overwritten 
        * in this problem, a stock can be bought or sold for multiple times in one day, so overwriting is not matter
        */
            dp(0) = dp(0) max (dp(1) + prices(i))
            dp(1) = dp(1) max (dp(0) - prices(i))
        }
        
        dp.max
        
    }
}