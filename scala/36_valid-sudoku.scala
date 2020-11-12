
/**
* select solution
* memo
*   1. three array recording whether current value is valid
*        1. rows array
*        2. columns array
*        3. blocks array
* time complexity: O(1), just one iteration
* space complexity: O(3), all sudoku are 9 x 9 
*/
object Solution0 {
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
      val size = board.length
      val rows = Array.ofDim[Boolean](size, size)
      val cols =  Array.ofDim[Boolean](size, size)
      val blocks =  Array.ofDim[Boolean](size, size)
      
      val coords = for(i <- board.indices.view; j <- board.indices.view; if board(i)(j) != '.') yield (i, j)
      
      coords.forall{ case (i, j) => 
          val num = board(i)(j).asDigit - 1
          val blockIdx = (i / 3) * 3 + (j / 3)
          if(!rows(i)(num) && !cols(j)(num) && !blocks(blockIdx)(num)){
            rows(i)(num) = true
            cols(j)(num) = true
            blocks(blockIdx)(num) = true
            true
        
          } else false
      }
  }
}

  
  /**
  *  recursive version : DFS
  *  memo
  *    1. three array recording whether current value is valid
  *        1. rows array
  *        2. columns array
  *        3. blocks array
  * time complexity: O(1), just one iteration
  * space complexity: O(3), all sudoku are 9 x 9
  */
  object Solution1 {

    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
      def _isValidSudoku(currentRow: Int, currentCol: Int, cols: Array[collection.mutable.Set[Char]], rows: Set[Char], blocks: Array[collection.mutable.Set[Char]]): Boolean = {
        (currentRow < board.length, currentCol < board.length) match {
          case (false, _) => true
          case (true, true) => // current line next position
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
          case (true, false) => _isValidSudoku(currentRow + 1, 0, cols, Set[Char](), blocks) // next line
        }
      }
      _isValidSudoku(0, 0, Array.fill(board.length)(collection.mutable.Set[Char]()), Set[Char](), Array.fill(board.length)(collection.mutable.Set[Char]()))
    }

  }



/**
* iterative
* memo
*   1. three array recording whether current value is valid
*        1. rows array
*        2. columns array
*        3. blocks array
* time complexity: O(1), just one iteration
* space complexity: O(3), all sudoku are 9 x 9
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

/**
* it's no need for zipWithIndex: faster
*/
object Solution2-2 {
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    val size = board.length
    val rows = Array.ofDim[Boolean](size, size)
    val cols =  Array.ofDim[Boolean](size, size)
    val blocks =  Array.ofDim[Boolean](size, size)


    var result = true
    for(i <- 0 until size; j <- 0 until size; if board(i)(j) != '.' && result) {
       val num = board(i)(j).asDigit - 1
       val blockIdx = (i / 3) * 3 + (j / 3)
      if(!rows(i)(num) && !cols(j)(num) && !blocks(blockIdx)(num)){
          rows(i)(num) = true
          cols(j)(num) = true
          blocks(blockIdx)(num) = true
          
      }else {
          result = false
      }

    }
    result
  }
}

/**
* function programming way without key word return in loop block
*/

object Solution2-3 {
    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
        val size = board.length
        val rows = Array.ofDim[Boolean](size, size)
        val cols =  Array.ofDim[Boolean](size, size)
        val blocks =  Array.ofDim[Boolean](size, size)
        
        val coords = for(i <- board.indices.view; j <- board.indices.view; if board(i)(j) != '.') yield (i, j)
        
        coords.forall{ case (i, j) => 
            val num = board(i)(j).asDigit - 1
            val blockIdx = (i / 3) * 3 + (j / 3)
            if(!rows(i)(num) && !cols(j)(num) && !blocks(blockIdx)(num)){
              rows(i)(num) = true
              cols(j)(num) = true
              blocks(blockIdx)(num) = true
              true
          
            } else false
        }
    }
}

