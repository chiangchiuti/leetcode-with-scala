/**
*  思路: 每個 row 上出現的 Q 的 column index 不會重複, 正斜線 (i + j) 和反斜線 ( i -j ) 上唯一
* 
* time complexity O(N^2)
*/
object Solution {
  def solveNQueens(n: Int): List[List[String]] = {
    val result = scala.collection.mutable.ListBuffer[List[Int]]()
    def _solveNQueens(queens: List[Int],
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

    _solveNQueens(List(), Set(), Set())

    result.map(b => plotResult(b, n)).toList
  }

  def plotResult(input: List[Int], n: Int): List[String] = {

    input.map(i =>  "." * i + "Q" + "." * (n - i - 1))
  }
}