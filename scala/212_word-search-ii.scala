/**
* hint: tries + dfs + pruning
*/
case class Node(childNode: Array[Node] = Array.ofDim[Node](26), var isWord: Boolean = false) {

  def apply(c: Char): Node = {
    this.apply(c.asDigit - 'a'.asDigit)
  }

  def apply(idx: Int): Node = {
    childNode(idx)
  }

  def update(idx: Int, node: Node): Unit = {
    childNode(idx) = node
  }

  def update(c: Char, node: Node): Unit = {
    this.update(c.asDigit - 'a'.asDigit, node)
  }
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


object Solution {
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

object Solution {

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

