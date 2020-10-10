/**
* 
*/

object Solution {
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
* a very genius and beautiful sol
*/
object Solution {
  def generateParenthesis(n: Int): List[String] = n match {
    case 0 => List("")
    case n =>
      for {
        m <- (0 to n - 1).toList
        x <- generateParenthesis(m)
        y <- generateParenthesis(n - 1 - m)
      } yield ("(" ++ x ++ ")" ++ y)
  }
}
