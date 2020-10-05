/**
* dynamic programming 
*  using an 3-dim array to store all previous status
*   [status index][k times transaction][buy or sell]
*  
*/
object Solution {
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
      val ii = (i + prices.length - 1) % profits.length
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
*/

object Solution1 {
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