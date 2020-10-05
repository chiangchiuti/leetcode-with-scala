/**
*  思路: 每個 row 上出現的 Q 的 column index 不會重複
* 
* time complexity O(N^2)
*/
object Solution {
  def solveNQueens(n: Int): List[List[String]] = {
    val result = scala.collection.mutable.ListBuffer[List[Int]]()
    def _solveNQueens(n: Int, queens: List[Int],
                      xy_diff: Set[Int],
                      xy_sum: Set[Int]
                     ): Unit = {

      val p = queens.size
      if (p == n) {
        result += queens
      } else {
        for (q <- 0 until n) {

          (queens.contains(q), xy_diff.contains(p - q), xy_sum.contains(p + q)) match {
            case (false, false, false) => _solveNQueens(n, queens :+ q, xy_diff + (p - q), xy_sum + (p + q))
            case _ =>
          }
        }
      }
    }

    _solveNQueens(n, List(), Set(), Set())
    plotResult(n, result)
  }


  def plotResult(n: Int, result: scala.collection.mutable.ListBuffer[List[Int]]): List[List[String]] = {

    val r = for {
      queen <- result
    } yield {
      for (q <- queen) yield
        "." * q + "Q" + "." * (n-q-1)
    }
    r.toList
  }
}