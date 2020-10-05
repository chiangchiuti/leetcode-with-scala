/**
* dynamic programming from bottom to up
*/
object Solution {
    import scala.util.Try
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val result = Array.ofDim[Array[Int]](triangle.size)
    triangle.zipWithIndex.foreach { case (ll, idx) => result(idx) = Array.ofDim[Int](ll.size) }
    for (i <- triangle.size - 1 to 0 by -1) {
      val inner = triangle(i)
      inner.zipWithIndex.foreach {
        case (v, j) =>
          val left = Try(result(i + 1)(j))
          val right = Try(result(i + 1)(j + 1))
          result(i)(j) = (left.getOrElse(0) min right.getOrElse(0)) + v
      }
    }

    Try(result.head.head).getOrElse(0)
  }
}


/**
* optimize
* without extra space for DP array , directly modify original space 
*  P.S. but u should convert collection from immutable List to mutable array
*/

object Solution1 {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val result = triangle.map(_.toArray).toArray
    for (i <- result.length - 2 to 0 by -1) {
      result(i).indices.foreach (j =>  result(i)(j) = ( result(i + 1)(j) min result(i + 1)(j + 1)) + result(i)(j))
    }
    result(0)(0)
  }
}


/**
* trick: dp is an array point to result's last array
*/

object Solution2 {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val result = triangle.map(_.toArray).toArray
    val dp = result.last
    for (i <- result.length - 2 to 0 by -1) {
      val ll = result(i)
      ll.indices.foreach (j =>  dp(j) = (dp(j) min dp(j + 1)) + ll(j))
    }
    dp(0)
  }
}