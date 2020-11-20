/**
* select solution
* 思路: 每個 row 上出現的 Q 的 column index 不會重複, 正斜線 (i + j) 和反斜線 ( i -j ) 上唯一
* memo:
*    1. using three extra collection
* time complexity O(N^2)
*/

object Solution0 {
  import scala.collection.mutable.ListBuffer
  def solveNQueens(n: Int): List[List[String]] = {

    val ans = ListBuffer[List[Int]]()
    _solveNQueens(n, List.empty[Int], Set.empty, Set.empty, ans)

    ans.map(plotResult).toList
  }
  private def plotResult(list: List[Int]): List[String] = list.map(v => "." * v + "Q" + "." * (list.size - v - 1))

  def _solveNQueens(n: Int, currentQueens: List[Int], slash: Set[Int], backSlash: Set[Int], ans: ListBuffer[List[Int]] ): Unit = {
    if(currentQueens.length == n) {
      ans += currentQueens
    }else{
      val rowIdx = currentQueens.length
      (0 until n)
        .filter(colIdx => checkValid((rowIdx, colIdx), currentQueens, slash, backSlash))
        .foreach(colIdx => _solveNQueens(n, currentQueens :+ colIdx, slash + (rowIdx - colIdx), backSlash + (rowIdx + colIdx), ans))
    }
  }

  def checkValid(coord: (Int, Int), currentQueens: List[Int], slash: Set[Int], backSlash: Set[Int]): Boolean = {
    val (rowIdx, colIdx) = coord
    !currentQueens.contains(colIdx) && !slash.contains(rowIdx - colIdx) && !backSlash.contains(rowIdx + colIdx)
  }
}


/**
* my first commitment
* 思路: 每個 row 上出現的 Q 的 column index 不會重複, 正斜線 (i + j) 和反斜線 ( i -j ) 上唯一
* memo:
*    1. using three extra collection
* time complexity O(N^2)
*/
object Solution1 {
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

object Solution1-2 {
  import scala.collection.mutable.ListBuffer
  def solveNQueens(n: Int): List[List[String]] = {

    val ans = ListBuffer[List[Int]]()
    _solveNQueens(n, List.empty[Int], Set.empty, Set.empty, ans)

    ans.map(plotResult).toList
  }
  private def plotResult(list: List[Int]): List[String] = list.map(v => "." * v + "Q" + "." * (list.size - v - 1))

  def _solveNQueens(n: Int, currentQueens: List[Int], slash: Set[Int], backSlash: Set[Int], ans: ListBuffer[List[Int]] ): Unit = {
    if(currentQueens.length == n) {
      ans += currentQueens
    }else{
      val rowIdx = currentQueens.length
      (0 until n)
        .filter(colIdx => checkValid((rowIdx, colIdx), currentQueens, slash, backSlash))
        .foreach(colIdx => _solveNQueens(n, currentQueens :+ colIdx, slash + (rowIdx - colIdx), backSlash + (rowIdx + colIdx), ans))
    }
  }

  def checkValid(coord: (Int, Int), currentQueens: List[Int], slash: Set[Int], backSlash: Set[Int]): Boolean = {
    val (rowIdx, colIdx) = coord
    !currentQueens.contains(colIdx) && !slash.contains(rowIdx - colIdx) && !backSlash.contains(rowIdx + colIdx)
  }
}