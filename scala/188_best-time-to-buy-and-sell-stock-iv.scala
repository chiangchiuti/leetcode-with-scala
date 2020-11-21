/**
* select solution
* dynamic programming
* memo 
*    1: using an 3-dim array to record previous state
*     dp definition: dp[2][j][l] means the best profit we can have at i-th day using EXACT j transactions and with/without stocks in hand.
*  time complexity: O(NK), N: the length of prices; k: transaction's restrictions
*  space complexity: O(K),  worst case: O(N)N
*/ 
object Solution0 {
    def maxProfit(k: Int, prices: Array[Int]): Int = {
        if(prices == null || prices.length < 2 || k < 1 ) return 0
        val kk = if(2 * k > prices.length) prices.length / 2 else k
        
        val dp = Array.tabulate(2, kk, 2) {
            case (_, _, 0) => Int.MinValue
            case (_, _, 1) => 0
            case _ => 0
        }
        
        for(i <- prices.indices; j <- 0 until kk){
            val current = i & 1
            val previous = current ^1
            // 0 for buy, 1 for sell
            dp(current)(j)(1) = dp(previous)(j)(1) max (dp(previous)(j)(0) + prices(i))
            dp(current)(j)(0) = dp(previous)(j)(0) max {
                if(j == 0) -prices(i)
                else dp(previous)(j - 1)(1) - prices(i)
            }
            
        }
        
        dp((prices.length - 1) & 1).map(_(1)).max
        
    }
}
/**
* my first commitment
* dynamic programming
* memo 
*    1: using an 3-dim array to record all previous state
*         dp[state index][k times transaction][buy or sell]
*     dp definition: dp[i][j][l] means the best profit we can have at i-th day using EXACT j transactions and with/without stocks in hand.
*  time complexity: O(NK), N: the length of prices; k: transaction's constraint
*/
object Solution1 {
  def maxProfit(k: Int, prices: Array[Int]): Int = {
    if(prices == null || prices.length < 2 || k < 1 ) return 0
    if(k * 2 >=  prices.length) return prices.sliding(2).collect{case arr if arr(1) > arr(0) => arr(1) - arr(0)}.sum
    val profits = Array.ofDim[Int](prices.length, k, 2)

    for{
      i <- profits.indices
      j <- 0 until k
    }{
      profits(i)(j)(0) = Int.MinValue  // hold
      profits(i)(j)(1) = 0 // sell
    }

    for {
      i <- prices.indices
      j <- 0 until k
    } {
      val ii = (i + prices.length - 1) % prices.length
      profits(i)(j)(1) = profits(ii)(j)(1)  max ( profits(ii)(j)(0) + prices(i)) // sell
      if (j > 0)
        profits(i)(j)(0) = profits(ii)(j)(0)  max ( profits(ii)(j - 1)(1) - prices(i)) // buy
      else
        profits(i)(j)(0) = profits(ii)(j)(0)  max  - prices(i) // buy
    }

    profits(prices.length - 1).map(_.max).max
  }
}

/**
* dp: decrease status array which only keep current and precious status
* memo
*    1. dp definition: dp[2][j][l] means the best profit we can have at i-th day using EXACT j transactions and with/without stocks in hand.
* time complexity: O(NK), N: the length of prices; k: transaction's constraint
* space complexity: O(K),  worst case: O(N)
*/

object Solution1-2 {
  def maxProfit(k: Int, prices: Array[Int]): Int = {
    if(prices == null || prices.length < 2 || k < 1 ) return 0
    if(k * 2 >=  prices.length) return prices.sliding(2).collect{case arr if arr(1) > arr(0) => arr(1) - arr(0)}.sum


    val profits = Array.ofDim[Int](2, k, 2)

    for{
      i <- profits.indices
      j <- 0 until k
    }{
      profits(i)(j)(0) = Int.MinValue  // hold
      profits(i)(j)(1) = 0 // sell
    }

    for {
      i <- prices.indices
      j <- 0 until k
    } {
      val currentI = (i + 1) % 2
      val preciousI = i % 2
      profits(currentI)(j)(1) = profits(preciousI)(j)(1)  max ( profits(preciousI)(j)(0) + prices(i)) // sell
      if (j > 0)
        profits(currentI)(j)(0) = profits(preciousI)(j)(0)  max ( profits(preciousI)(j - 1)(1) - prices(i)) // buy
      else
        profits(currentI)(j)(0) = profits(preciousI)(j)(0)  max  - prices(i) // buy
    }
    profits(prices.length % 2).map(_.max).max // prices.length % 2: decide the newest status index
  }

  private def debugProfits(profits: Array[Array[Array[Int]]]): Unit = {
        profits.zipWithIndex.foreach{
          case (p, i) =>
            println(s"status: $i")
            p.zipWithIndex.foreach{
            case (pp, j) =>
                println(s"transaction $j: hold: ${pp(0)}, sell: ${pp(1)}")
          }
            println(" ")
        }
  }
}
/**
* dp: decrease status array which only keep current and precious status
* memo
*    1. dp definition: dp[2][j][l] means the best profit we can have at i-th day using EXACT j transactions and with/without stocks in hand.
* time complexity: O(NK), N: the length of prices; k: transaction's constraint
* space complexity: O(K),  worst case: O(N)
*/
object Solution1-3 {
    def maxProfit(k: Int, prices: Array[Int]): Int = {
        if(prices == null || prices.length < 2 || k < 1 ) return 0
        val kk = if(2 * k > prices.length) prices.length / 2 else k
        
        val dp = Array.tabulate(2, kk, 2) {
            case (_, _, 0) => Int.MinValue
            case (_, _, 1) => 0
            case _ => 0
        }
        
        for(i <- prices.indices; j <- 0 until kk){
            val current = i & 1
            val previous = current ^1
            // 0 for buy, 1 for sell
            dp(current)(j)(1) = dp(previous)(j)(1) max (dp(previous)(j)(0) + prices(i))
            dp(current)(j)(0) = dp(previous)(j)(0) max {
                if(j == 0) -prices(i)
                else dp(previous)(j - 1)(1) - prices(i)
            }
            
        }
        
        dp((prices.length - 1) & 1).map(_(1)).max
        
    }
}