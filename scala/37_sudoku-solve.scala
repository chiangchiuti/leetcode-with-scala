/**
* selected solution
* DFS + pruning + queue
* time complexity: O(N^2)
*/

object Solution0 {
  def solveSudoku(board: Array[Array[Char]]): Unit = {

    val coords = for(i <- board.indices.toList; j <- board.indices; if board(i)(j) == '.') yield (i, j)
    if (!solveSudoku(board.map(_.clone), coords, board))
      println("cannot solve under this condition")
  }

  private def solveSudoku(board: Array[Array[Char]], coordQueue: List[(Int, Int)] , ansBoard: Array[Array[Char]]): Boolean = {
    coordQueue match {
      case coord :: newQueue if coordQueue.nonEmpty =>
        ('1' to '9').filter(isValid(board, coord, _)).exists{ char =>
          board(coord._1)(coord._2) = char
          val ret = solveSudoku(board, newQueue, ansBoard)
          board(coord._1)(coord._2) = '.'
          ret
        }
      case _ if coordQueue.isEmpty =>
        board.zipWithIndex.foreach{case (arr: Array[Char], idx: Int) => ansBoard(idx) = arr.clone()}
        true
      case _ =>
        false
    }
  }

  private def isValid(board: Array[Array[Char]], coord: (Int, Int), value: Char): Boolean = {
    val (rowIdx, colIdx) = coord
    val rowValid = ! board(rowIdx).contains(value)
    val columnValid = board.forall(row => row(colIdx) != value)
    val blockValid = generateBlockIdx(rowIdx, colIdx) forall  {case (r, c) => board(r)(c) != value}

    rowValid && columnValid && blockValid
  }

  private def generateBlockIdx(rowIdx: Int, colIdx: Int): Iterator[(Int, Int)] = {
    val blockRowIdx = (rowIdx / 3) * 3
    val blockColIdx = (colIdx / 3) * 3
    for(i <- (blockRowIdx until blockRowIdx + 3).toIterator ; j <- blockColIdx until blockColIdx + 3) yield (i,j)
  }
}



/**
* my first commitment
* DFS + pruning
*/
object Solution1 {
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
*  DFS + pruning + queue
*  memo:
*    using a queue storing unfilled index
*/
object Solution1-2 {

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
      // end condition
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
*  DFS + pruning + queue
*/
object Solution1-3 {

  import scala.collection.immutable.Queue

  def solveSudoku(board: Array[Array[Char]]): Unit = {
    val indices = scala.collection.mutable.Queue[(Int, Int)]()

    for (i <- 0 until board.length; j <- 0 until board.length) {
      val v = board(i)(j)
      if (v == '.') indices.enqueue((i, j))
    }
    _solveSudoku(Queue(indices.dequeueAll(_ => true): _*), board.map(_.clone()), board)
  }


  def _solveSudoku(indices: Queue[(Int, Int)], currentBoard: Array[Array[Char]], finalBoard: Array[Array[Char]]): Boolean = {
    if (indices.isEmpty) {
      currentBoard.zipWithIndex.foreach { case (a, idx) => a.copyToArray(finalBoard(idx)) }
      return true
    }

    val ((row, col), newIndices) = indices.dequeue
    ('1' to '9').filter(_checkValid(_, (row, col), currentBoard)).find { // find: 找出第一個合法數字，代表其後的迭代有解
      c =>
        currentBoard(row)(col) = c
        if (_solveSudoku(newIndices, currentBoard, finalBoard)) true
        else {
          currentBoard(row)(col) = '.'
          false
        }
    } match {
      case Some(_) => true
      case None => false  // 這個盤勢不管填什麼後續都無解
    }
  }

  def _checkValid(c: Char, index: (Int, Int), currentBoard: Array[Array[Char]]): Boolean = {
    val (row, col) = index
    val blockRowIdx = 3 * (row / 3)
    val blockColIdx = 3 * (col / 3)
    val checkBoard = (rowAnchar: Int, colAnchar: Int) => {
      val pairs = for (i <- 0 until 3; j <- 0 until 3) yield (rowAnchar + i, colAnchar + j)
      pairs.exists { case (i, j) => currentBoard(i)(j) == c }
    }
    if (currentBoard(row).contains(c) || currentBoard.exists(a => a(col) == c) || checkBoard(blockRowIdx, blockColIdx)) false
    else true
  }
}

/**
*  DFS + pruning + queue
*    improvement: isValid is more concise
*/
object Solution1-4 {
  def solveSudoku(board: Array[Array[Char]]): Unit = {

    val coords = for(i <- board.indices.toList; j <- board.indices; if board(i)(j) == '.') yield (i, j)
    if (!solveSudoku(board.map(_.clone), coords, board))
      println("cannot solve under this condition")
  }

  private def solveSudoku(board: Array[Array[Char]], coordQueue: List[(Int, Int)] , ansBoard: Array[Array[Char]]): Boolean = {
    coordQueue match {
      case coord :: newQueue if coordQueue.nonEmpty =>
        ('1' to '9').filter(isValid(board, coord, _)).exists{ char =>
          board(coord._1)(coord._2) = char
          val ret = solveSudoku(board, newQueue, ansBoard)
          board(coord._1)(coord._2) = '.'
          ret
        }
      case _ if coordQueue.isEmpty =>
        board.zipWithIndex.foreach{case (arr: Array[Char], idx: Int) => ansBoard(idx) = arr.clone()}
        true
      case _ =>
        false
    }
  }

  private def isValid(board: Array[Array[Char]], coord: (Int, Int), value: Char): Boolean = {
    val (rowIdx, colIdx) = coord
    val rowValid = ! board(rowIdx).contains(value)
    val columnValid = board.forall(row => row(colIdx) != value)
    val blockValid = generateBlockIdx(rowIdx, colIdx) forall  {case (r, c) => board(r)(c) != value}

    rowValid && columnValid && blockValid
  }

  private def generateBlockIdx(rowIdx: Int, colIdx: Int): Iterator[(Int, Int)] = {
    val blockRowIdx = (rowIdx / 3) * 3
    val blockColIdx = (colIdx / 3) * 3
    for(i <- (blockRowIdx until blockRowIdx + 3).toIterator ; j <- blockColIdx until blockColIdx + 3) yield (i,j)
  }
}



object Solution2 {
  def solveSudoku(board: Array[Array[Char]]): Unit = {

    _solveSudoku(0, 0, board.map(_.clone()), board)
  }

  def _solveSudoku(currentRow: Int, currentCol: Int, currenBboard: Array[Array[Char]], finalBoard: Array[Array[Char]]): Boolean = {
    (currentRow < finalBoard.length, currentCol < finalBoard.length) match {
      case (false, _) => // end condition
        currenBboard.zipWithIndex.foreach { case (a, idx) => a.copyToArray(finalBoard(idx)) }
        true
      case (true, false) => // next line (row)
        _solveSudoku(currentRow + 1, 0, currenBboard, finalBoard)
      
      case (true, true) if currenBboard(currentRow)(currentCol) == '.' => 
        ('1' to '9').filter(c => _isValid(currentRow, currentCol, c, currenBboard))
          .find(c => _solveSudoku(currentRow , currentCol + 1, copyBoard(currenBboard)(currentRow, currentCol, c), finalBoard)) match { // fix row shift col
          case Some(_) => true
          case None => false
        }

      case _ => _solveSudoku(currentRow, currentCol + 1, currenBboard, finalBoard) // fix row, next col 
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
* DFS + pruning + extra space
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

object Solution4-2 {
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    /* three extra tables to record whether coordinate is occupied */
    val columns = Array.ofDim[Boolean](board.length, board.length)
    val rows = Array.ofDim[Boolean](board.length, board.length)
    /**
      * block index:
      *     1 2 3
      *     4 5 6
      *     7 8 9
      * convert (rowIndex, columnIndex) to blockIndex:  ( rowIndex / 3 ) * 3 + ( columnIndex / 3)
      */
    val blocks = Array.ofDim[Boolean](board.length, board.length)

    /* DFS worker*/
    def _solveSudoku(board: Array[Array[Char]], coordQueue: List[(Int, Int)], ans: Array[Array[Char]], checkValid: ((Int, Int), Char) => Boolean): Boolean = {
      coordQueue match {
          /* DFS not complete case : coordinate queue non empty */
        case coord :: newQueue if coordQueue.nonEmpty =>
          ('1' to '9').filter(checkValid(coord, _)).exists { char =>
            val (rowIdx, colIdx) = coord

            /* set board with char value by coordinate */
            board(rowIdx)(colIdx) = char
            rows(rowIdx)(char.asDigit - 1) = true
            columns(colIdx)(char.asDigit - 1) = true
            blocks((rowIdx / 3) * 3 + (colIdx / 3))(char.asDigit - 1) = true

            val ret = _solveSudoku(board, newQueue, ans, checkValid)

            /* recover to status before calling  _solveSudoku 
            *  reset board table, rows table, columns table and blocks table
            * */
            board(rowIdx)(colIdx) = '.'
            rows(rowIdx)(char.asDigit - 1) = false
            columns(colIdx)(char.asDigit - 1) = false
            blocks((rowIdx / 3) * 3 + (colIdx / 3))(char.asDigit - 1) = false
            ret
          }

          /* coordinate queue ran out, answer should shows up */
        case _ if coordQueue.isEmpty =>
          board.zipWithIndex.foreach { case (r, idx) => ans(idx) = r.clone }
          true

        case _ => false
      }
    }

    /* generate all empty coordinates */
    val coords = for (i <- board.indices.toList; j <- board.indices; if board(i)(j) == '.') yield (i, j)

    /* initial rows table, columns table, blocks table */
    for (i <- board.indices.toList; j <- board.indices; if board(i)(j) != '.') {
      val charIdx = board(i)(j).asDigit - 1  // index range from 0 to 9
      rows(i)(charIdx) = true
      columns(j)(charIdx) = true
      blocks((i / 3) * 3 + (j / 3))(charIdx) = true
    }
    val isValidFunc = isValid(_, _, rows, columns, blocks)

    _solveSudoku(board.map(_.clone), coords, board, isValidFunc)
  }

  /* check input char value is valid at the coordinate */
  def isValid(coord: (Int, Int), value: Char, rows: Array[Array[Boolean]], columns: Array[Array[Boolean]], blocks: Array[Array[Boolean]]): Boolean = {
    val (row, col) = coord
    val charIdx = value.asDigit - 1
    val blockIdx = (row / 3) * 3 + (col / 3)
    !rows(row)(charIdx) && !columns(col)(charIdx) && !blocks(blockIdx)(charIdx)
  }
}