object Solution {
  def solveSudoku(board: Array[Array[Char]]): Unit = {

    _solveSudoku(board)
  }


  def _solveSudoku(board: Array[Array[Char]]): Boolean = {

    for {
      (rows, rowIdx) <- board.zipWithIndex
      (v, colIdx) <- rows.zipWithIndex
    } {
      if (v == '.') {
        for (c <- '1' to '9') {

          if (_isValid(rowIdx, colIdx, c, board)) {
            board(rowIdx)(colIdx) = c
            if (_solveSudoku(board)) return true
            else board(rowIdx)(colIdx) = '.'
          }
        }
        return false
      }
    }
    true
  }

  def _isValid(row: Int, col: Int, char: Char, board: Array[Array[Char]]): Boolean = {

    val boardRowIndex = 3 * (row / 3)
    val boardColIndex = 3 * (col / 3)
    if (board(row).contains(char)
      || board.exists(r => r(col) == char)
      || board.slice(boardRowIndex, boardRowIndex + 3).map(_.slice(boardColIndex, boardColIndex + 3)).exists(r => r.contains(char))) false
    else true
  }

}

/**
* optimize
*/

object Solution2 {
  def solveSudoku(board: Array[Array[Char]]): Unit = {

    _solveSudoku(0, 0, board.map(_.clone()), board)
  }

  def _solveSudoku(currentRow: Int, currentCol: Int, currenBboard: Array[Array[Char]], finalBoard: Array[Array[Char]]): Boolean = {
    (currentRow < finalBoard.length, currentCol < finalBoard.length) match {
      case (false, _) =>
        currenBboard.zipWithIndex.foreach { case (a, idx) => a.copyToArray(finalBoard(idx)) }
        true
      case (true, false) =>
        _solveSudoku(currentRow + 1, 0, currenBboard, finalBoard)
      case (true, true) if currenBboard(currentRow)(currentCol) == '.' =>
        ('1' to '9').filter(c => _isValid(currentRow, currentCol, c, currenBboard))
          .find(c => _solveSudoku(currentRow , currentCol + 1, copyBoard(currenBboard)(currentRow, currentCol, c), finalBoard)) match {
          case Some(_) => true
          case None => false
        }

      case _ => _solveSudoku(currentRow, currentCol + 1, currenBboard, finalBoard)
    }
  }

  val copyBoard = (b: Array[Array[Char]]) => (row: Int, col: Int, c: Char) => {
    val newB = b.map(_.clone())
    newB(row)(col) = c
    newB
  }

  def _isValid(row: Int, col: Int, char: Char, board: Array[Array[Char]]): Boolean = {

    val checkBoardExits = (rr: Int, cc: Int, c: Char) => {
      var result = false
      for {
        i <- 0 until 3
        j <- 0 until 3
        if !result
      } {
        if (board(i + rr)(j + cc) == c) result = true
      }
      result
    }
    val boardRowIndex = 3 * (row / 3)
    val boardColIndex = 3 * (col / 3)
    if (board(row).contains(char)
      || board.exists(r => r(col) == char)
      || checkBoardExits(boardRowIndex, boardColIndex, char)) {
      false
    } else {
      true
    }
  }

}

/**
*  optimize with queue 
*/
object Solution3 {

  import scala.collection.immutable.Queue

  def solveSudoku(board: Array[Array[Char]]): Unit = {

    val indexes = scala.collection.mutable.Queue[(Int, Int)]()
    for {
      (row, rowIdx) <- board.zipWithIndex
      (value, colIdx) <- row.zipWithIndex
    } {
      if (value == '.') {
        indexes.enqueue((rowIdx, colIdx))
      }
    }
    _solveSudoku(Queue(indexes.dequeueAll(_  => true): _*), board.map(_.clone()), board)
  }


  def _solveSudoku(indexes: Queue[(Int, Int)], currentBoard: Array[Array[Char]], finalBoard: Array[Array[Char]]): Boolean = {
    if (indexes.isEmpty) {
      currentBoard.zipWithIndex.foreach { case (a, idx) => a.copyToArray(finalBoard(idx)) }
      true
    } else {
      val ((row, col), newIndexes) = indexes.dequeue
      ('1' to '9').filter(_isValid(row, col, _, currentBoard)).find ( c =>_solveSudoku(newIndexes, copyBoard(currentBoard)(row, col, c), finalBoard))
       match {
        case Some(_) => true
        case None => false
      }
    }

  }
  def _isValid(row: Int, col: Int, char: Char, board: Array[Array[Char]]): Boolean = {

    val checkBoardExits = (rr: Int, cc: Int, c: Char) => {
      var result = false
      for {
        i <- 0 until 3
        j <- 0 until 3
        if !result
      } {
        if (board(i + rr)(j + cc) == c) result = true
      }
      result
    }
    val boardRowIndex = 3 * (row / 3)
    val boardColIndex = 3 * (col / 3)
    if (board(row).contains(char)
      || board.exists(r => r(col) == char)
      || checkBoardExits(boardRowIndex, boardColIndex, char)) {
      false
    } else {
      true
    }
  }

  val copyBoard = (b: Array[Array[Char]]) => (row: Int, col: Int, c: Char) => {
    val newB = b.map(_.clone())
    newB(row)(col) = c
    newB
  }
}


/**
* using extra three two dimension array to store col row and block's information
* a mutable collection method
*/


object Solution4 {
  import scala.reflect.ClassTag
  import scala.collection.immutable.Queue
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    val indexes = scala.collection.mutable.Queue[(Int, Int)]()
    val rows = Array.ofDim[Boolean](board.length, board.length)
    val cols = Array.ofDim[Boolean](board.length, board.length)
    val blocks = Array.ofDim[Boolean](board.length, board.length)
    for {
      (row, rowIdx) <- board.zipWithIndex
      (value, colIdx) <- row.zipWithIndex
    } {
      if (value == '.') {
        indexes.enqueue((rowIdx, colIdx))
      } else {
        val blockIdx = 3 * (rowIdx / 3) + (colIdx / 3)
        val v = value.asDigit - 1
        rows(rowIdx)(v) = true
        cols(colIdx)(v) = true
        blocks(blockIdx)(v) = true

      }

    }

    _solveSudoku( Queue(indexes.dequeueAll(_ => true): _*),
      rows,
      cols,
      blocks,
      board
    )

  }

  def _solveSudoku(indexes: Queue[(Int, Int)],
                   rows: Array[Array[Boolean]],
                   cols: Array[Array[Boolean]],
                   blocks: Array[Array[Boolean]],
                   currentBoard: Array[Array[Char]]
                  ): Boolean = {
    if (indexes.isEmpty) {
//      currentBoard.zipWithIndex.foreach { case (a, idx) => a.copyToArray(finalBoard(idx)) }
      true
    } else {
      val ((row, col), newIndexes) = indexes.dequeue
      ('1' to '9').filter(_isValid((row, col), _, rows, cols, blocks))
        .find { c =>
          val v = c.asDigit - 1
          rows(row).update(v, true)
          cols(col).update(v, true)
          blocks( 3 * (row / 3) + (col / 3)).update(v, true)
          currentBoard(row)(col) = c
          if (_solveSudoku(newIndexes,
            rows,
            cols,
            blocks,
            currentBoard
            //            copyBoard(rows)(row, v, true),
            //            copyBoard(cols)(col, v, true),
            //            copyBoard(blocks)(3 * (row / 3) + (col / 3), v, true),
            //            copyBoard(currentBoard)(row, col, c),
          //  finalBoard
          )) {
            true
          } else {
            rows(row).update(v, false)
            cols(col).update(v, false)
            blocks( 3 * (row / 3) + (col / 3)).update(v, false)
            currentBoard(row)(col) = '.'
            false
          }
        } match {
        case Some(_) => true
        case None => false
      }
    }
  }
  def _isValid(index: (Int, Int),
               char: Char,
               rows: Array[Array[Boolean]],
               cols: Array[Array[Boolean]],
               blocks: Array[Array[Boolean]]): Boolean = {


    val (row, col) = index
    val v = char.asDigit - 1
    val blockIdx = 3 * (row / 3) + (col / 3)

    if (rows(row)(v) || cols(col)(v) || blocks(blockIdx)(v)) false
    else true
  }

//  def copyBoard[T](b: Array[Array[T]])(row: Int, col: Int, c: T)(implicit ctg: ClassTag[T]): Array[Array[T]] = {
//    val newB = b.map(_.clone())
//    newB(row)(col) = c
//    newB
//  }
}