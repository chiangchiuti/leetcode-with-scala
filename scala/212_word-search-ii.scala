
/**
* chosen solution
* tries + dfs + pruning
*    put all words into tries
*    DFS way searching all char in board composing a word and searching whether the word exists in tries
*/
object Solution0 {
  private val visitedLabel = '#'
  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val tries = new Trie()
    words.foreach(tries.insert)
    dfs(tries, board)
  }


  def dfs(tries: Trie, board: Array[Array[Char]]): List[String] = {
    def _dfs(coord: (Int, Int), prePrefix: String, board: Array[Array[Char]], ret: scala.collection.mutable.HashSet[String]): Unit = {
      val currentChar =  board(coord._1)(coord._2)
      val newPrefix = prePrefix + currentChar
      if(tries.search(newPrefix)) ret += newPrefix
      /* pruning */
      if(tries.startsWith(newPrefix)){
        board(coord._1)(coord._2) = visitedLabel
        getAvailableCoords(coord, (board.length, board(0).length)).foreach {
          case (r, c) if board(r)(c) != visitedLabel => _dfs((r, c), newPrefix, board, ret)
          case _ =>
        }
        board(coord._1)(coord._2) = currentChar
      }
    }

    val coords = for(i <- board.indices; j <- board(0).indices) yield (i, j)
    val ret = scala.collection.mutable.HashSet[String]()
    coords.foreach(coord => _dfs(coord, "", board, ret))
    ret.toList

  }

  private val getAvailableCoords = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    List(
      (row + 1, col),
      (row, col + 1),
      (row - 1, col),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 &&  0 <= c && c < shape._2}
  }
}

/**
* hint: tries + dfs + pruning
*/

object Solution1-1 {
  val result = scala.collection.mutable.Set[String]()

  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    result.clear()
    val tries = new Trie()
    tries.insert(words)
    for{
      row <- board.indices
      col <- board(0).indices
    }{
      _dfs(board, Array.ofDim[Boolean](board.length, board(0).length), tries, "", (row, col))
    }

    result.toList
  }

  private def _dfs(board: Array[Array[Char]], seenBoard:Array[Array[Boolean]], tries: Trie, currentPrefix: String, currentIdx: (Int, Int)) {
    val (row, col) = currentIdx
    val newPrefix = currentPrefix + board(row)(col)

    if (tries.search(newPrefix))
      result += newPrefix

    if(tries.startsWith(newPrefix)){
      seenBoard(row)(col) = true
      getNextPosition(currentIdx, seenBoard).foreach{idx =>
        _dfs(board, seenBoard.map(_.clone()), tries, newPrefix, idx)
      }
    }
  }

  private def getNextPosition(currentIdx: (Int, Int), seenBoard: Array[Array[Boolean]]): Array[(Int, Int)] = {

    def check(row: Int, col: Int): Boolean = {
      if(row >= seenBoard.length || row < 0 || col >= seenBoard(0).length || col < 0 || seenBoard(row)(col))  false
      else true
    }
    val (row, col) = currentIdx
    val result = scala.collection.mutable.ArrayBuffer[(Int, Int)]()

    for{
      i <- -1 to 1
      j <- -1 to 1
    }{
      if((math.abs(i) + math.abs(j) == 1) && check(row + i, col + j)) result.append((row + i, col + j))
    }
    result.toArray
  }
}

/**
* simplify : without seen matrix
*/

object Solution1-2 {

  val result = scala.collection.mutable.Set[String]()
  private val inBounds = (shape: (Int, Int)) => (coord: (Int, Int)) => coord._1 < shape._1 && coord._1 >= 0 && coord._2 < shape._2 && coord._2 >= 0
  private val getNeighbors = (coord: (Int, Int), filter: ((Int, Int)) => Boolean) => {
    List(
      (coord._1 + 1, coord._2),
      (coord._1, coord._2 + 1),
      (coord._1 - 1, coord._2),
      (coord._1, coord._2 - 1)
    ).filter(filter)
  }

  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    result.clear()
    val tries = new Trie()
    tries.insert(words)
    for {
      row <- board.indices
      col <- board(0).indices
    } {
      _dfs(board, tries, "", (row, col))
    }
    result.toList
  }

  private def _dfs(board: Array[Array[Char]], tries: Trie, currentPrefix: String, coord: (Int, Int)) {

    val (row, col) = coord
    val newPrefix = currentPrefix + board(row)(col)

    if (tries.search(newPrefix))
      result += newPrefix

    val c = board(row)(col)
    board(row)(col) = '#'
    if (tries.startsWith(newPrefix)) {

      getNeighbors(coord, inBounds((board.length, board.head.length))(_))
        .foreach {
          case (nr, nc) if board(nr)(nc) != '#' =>
            _dfs(board, tries, newPrefix, (nr, nc))
          case _ =>
        }
    }
    board(row)(col) = c
  }
}


object Solution1-3 {
  private val visitedLabel = '#'
  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val tries = new Trie()
    words.foreach(tries.insert)
    dfs(tries, board)
  }


  def dfs(tries: Trie, board: Array[Array[Char]]): List[String] = {
    def _dfs(coord: (Int, Int) ,prePrefix: String, board: Array[Array[Char]], ret: scala.collection.mutable.HashSet[String]): Unit = {
      val currentChar =  board(coord._1)(coord._2)
      val newPrefix = prePrefix + currentChar
      if(tries.search(newPrefix)) ret += newPrefix
      /* pruning */
      if(tries.startsWith(newPrefix)){
        board(coord._1)(coord._2) = visitedLabel
        getAvailableCoords(coord, (board.length, board(0).length)).foreach {
          case (r, c) if board(r)(c) != visitedLabel => _dfs((r, c), newPrefix, board, ret)
          case _ =>
        }
        board(coord._1)(coord._2) = currentChar
      }
    }

    val coords = for(i <- board.indices; j <- board(0).indices) yield (i, j)
    val ret = scala.collection.mutable.HashSet[String]()
    coords.foreach(coord => _dfs(coord, "", board, ret))
    ret.toList

  }

  private val getAvailableCoords = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    List(
      (row + 1, col),
      (row, col + 1),
      (row - 1, col),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 &&  0 <= c && c < shape._2}
  }
}


/** helper class **/
case class Node(childNode: Array[Node] = Array.ofDim[Node](26), var isWord: Boolean = false) {
  def apply(c: Char): Node = this.apply(c.asDigit - 'a'.asDigit)
  def apply(idx: Int): Node =  childNode(idx)
  def update(idx: Int, node: Node): Unit = childNode(idx) = node
  def update(c: Char, node: Node): Unit = this.update(c.asDigit - 'a'.asDigit, node)
}


class Trie() {

  /** Initialize your data structure here. */
  val root = Node()


  /** Inserts a word into the trie. */
  def insert(word: String) {
    var node = root
    word.foreach {
      case c if node(c) == null =>
        node(c) = Node()
        node = node(c)

      case c => node = node(c)
    }
    node.isWord = true

  }
  def insert(words: Array[String]): Unit = {
    words.foreach(insert)
  }

  /** Returns if the word is in the trie. */
  def search(word: String): Boolean = {
    searchUtil(word).exists(_.isWord)
  }

  /** Returns if there is any word in the trie that starts with the given prefix. */
  def startsWith(prefix: String): Boolean = {
    searchUtil(prefix).isDefined
  }


  private def searchUtil(s: String): Option[Node] = {
    var node = root

    s.foreach {
      case c if node(c) != null => node = node(c)
      case _ => return None
    }
    Some(node)
  }
  def traversal(): Unit = {
    val result = scala.collection.mutable.ListBuffer[String]()

    def _traversal(prefix: String, node: Node): Unit = {
      if(node.isWord) {
        result += prefix
      }
      node.childNode.zipWithIndex.foreach{
        case (n, idx) if n != null => _traversal(prefix + ('a' + idx).toChar, n)
        case _ =>
      }

    }
    _traversal("", root)
    result.foreach(s => println(s.mkString("")))

  }
}