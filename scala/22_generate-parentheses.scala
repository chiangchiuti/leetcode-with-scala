/**
* my first commitment 
* DFS + backtracking
* time complexity： O(4^n / square(n))
*     n-th Catalan number
*/

object Solution1 {
  def generateParenthesis(n: Int): List[String] = {
    val buffer = scala.collection.mutable.ListBuffer[String]()
    val l = "("
    val r = ")"

    def _generateParenthesis(right: Int, left: Int, n: Int, pair: String) {

      if (right == n && left == n) {
        buffer += pair
      } else {
        if (left < n) _generateParenthesis(right, left + 1, n, pair + l) // you can add open whenever you want if it's smaller then n
        if (left > right && right < n) _generateParenthesis(right + 1, left, n, pair + r)
      }
    }
    _generateParenthesis(0, 0, n, "")
    buffer.toList
  }
}

/**
* closure number
* a very genius and beautiful sol
*/
object Solution2 {
  def generateParenthesis(n: Int): List[String] =
    n match {
      case 0 => List("")
      case _ =>
        for{
          m <- (0 until n).toList  // ensure yield type is List instead of indexSeq
          leftString <- generateParenthesis(m)
          rightString <- generateParenthesis(n - m - 1)
        } yield "(" ++ leftString ++ ")" ++ rightString
    }
}
