  /**
  * recursive version
  */
  object Solution {

    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
      def _isValidSudoku(currentRow: Int, currentCol: Int, cols: Array[collection.mutable.Set[Char]], rows: Set[Char], blocks: Array[collection.mutable.Set[Char]]): Boolean = {
        (currentRow < board.length, currentCol < board.length) match {
          case (false, _) => true
          case (true, true) =>
            val v = board(currentRow)(currentCol)
            val blockIndex = 3 * (currentRow / 3) + currentCol / 3
            if (v == '.') {
              _isValidSudoku(currentRow, currentCol + 1, cols, rows, blocks)

            } else {
              if (cols(currentCol).contains(v) || rows.contains(v) || blocks(blockIndex).contains(v)) {
                false
              }
              else {
                blocks(blockIndex) += v
                cols(currentCol) += v
                _isValidSudoku(currentRow, currentCol + 1, cols, rows + v, blocks)
              }
            }
          case (true, false) => _isValidSudoku(currentRow + 1, 0, cols, Set[Char](), blocks)
        }
      }
      _isValidSudoku(0, 0, Array.fill(board.length)(collection.mutable.Set[Char]()), Set[Char](), Array.fill(board.length)(collection.mutable.Set[Char]()))
    }

  }



/**
* iterative
* using 3 two-dimension array to store if num exits or not among row column and block
*/
object Solution2 {
    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
        val rows = Array.ofDim[Boolean](board.length, board.length)
        val cols = Array.ofDim[Boolean](board.length, board.length)
        val blocks = Array.ofDim[Boolean](board.length, board.length)
        var result = true
        for {
            (row, rowIndex) <- board.zipWithIndex
            (v, colIndex) <- row.zipWithIndex
            if result
        } {
            if (v != '.') {
            val blockIndex = 3 * (rowIndex / 3) + (colIndex / 3)
            val value = v.asDigit - 1
            if (rows(rowIndex)(value) || cols(colIndex)(value) || blocks(blockIndex)(value)) {
                result = false
            } else {
                rows(rowIndex)(value) = true
                cols(colIndex)(value) = true
                blocks(blockIndex)(value) = true
            }
            }

        }
            result

        }
}

