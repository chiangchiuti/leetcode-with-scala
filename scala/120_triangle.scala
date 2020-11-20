/**
* selection solution
* dynamic programming - bottom up
*     state definition: dp(j) represents minimum sum at point triangle(i)(j) during bottom up 
* time complexity: O(N) N is the height of triangle
* space complexity: O(N), only create dp array with dimension of last triangle
*/

object Solution0 {
    def minimumTotal(triangle: List[List[Int]]): Int = {
        val depth = triangle.size
        val dp = triangle.last.toArray
        for(i <- (depth - 2) to 0 by -1; j <- triangle(i).indices) {
            dp(j) = triangle(i)(j) + (dp(j) min dp(j + 1)) 
        }
        dp(0)
    }
}

/**
* my first commitment
* memo
*    dynamic programming from bottom to up
* time complexity: O(N) N is the height of triangle
* space complexity: O(N^2) : (1 + N) * N  / 2
*/
object Solution1 {
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
* dp dimension is like triangle
* time complexity: O(N) N is the height of triangle
* space complexity: O(N^2) : (1 + N) * N  / 2
*/

object Solution1-1 {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val result = triangle.map(_.toArray).toArray // O(N^2)
    for (i <- result.length - 2 to 0 by -1) {
      result(i).indices.foreach (j =>  result(i)(j) = ( result(i + 1)(j) min result(i + 1)(j + 1)) + result(i)(j))
    }
    result(0)(0)
  }
}


/**
* trick: dp is an array point to copy version of result's last array
* time complexity: O(N) N is the height of triangle
* space complexity: O(N^2) : (1 + N) * N  / 2
*/

object Solution1-2 {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val result = triangle.map(_.toArray).toArray  // O(N^2)
    val dp = result.last
    for (i <- result.length - 2 to 0 by -1) {
      val ll = result(i)
      ll.indices.foreach (j =>  dp(j) = (dp(j) min dp(j + 1)) + ll(j))
    }
    dp(0)
  }
}

/**
* optimize from above: without covert entire list to array
* time complexity: O(N) N is the height of triangle
* space complexity: O(N), only create dp array with dimension of last triangle
*/
object Solution1-3 {
    def minimumTotal(triangle: List[List[Int]]): Int = {
        val depth = triangle.size
        val dp = triangle.last.toArray
        for(i <- (depth - 2) to 0 by -1; j <- triangle(i).indices) {
            dp(j) = triangle(i)(j) + (dp(j) min dp(j + 1)) 
        }
        dp(0)
    }
}