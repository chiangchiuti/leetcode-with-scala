
/**
* chosen solution
* tries + dfs + pruning
* memo
*   1. put all words into tries which is implemented by hashmap
*   2. DFS way searching all char in board composing a word and searching whether the word exists in tries
*   3. in dfs, we directly input the node from tries instead of tries itself 
*   4. pruning an edge after matching a word and its children couldn't represent a word
*/
import scala.collection.mutable
case class Node(next: mutable.Map[Char, Node] = mutable.Map.empty, var isWord: Boolean = false){
  def apply(char: Char): Option[Node] = next.get(char)
  def update(char: Char, node: Node): Unit = next(char) = node
}

class Tries(){
  val root = Node()
  def insert(word: String): Unit = {
    var node = root
    word.foreach { c =>
      node(c) match {
        case Some(n) => node = n
        case None =>
          node(c) = Node()
          node = node(c).get
      }
    }
    node.isWord = true
  }

  def startsWith(prefix: String): Boolean = searchUtil(prefix).isDefined
  def search(word: String): Boolean =  searchUtil(word).exists(_.isWord)

  def searchUtil(s: String): Option[Node] = {
    var node = root
    s.foreach { c =>
      node(c) match {
        case Some(n) => node = n
        case None => return None
      }
    }
    Some(node)
  }
}


object Solution0 {
  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val tries = new Tries()
    words.foreach(tries.insert)
    dfs(tries, board)
  }

  def dfs(tries: Tries, board: Array[Array[Char]]): List[String] = {
    def _dfs(coord: (Int, Int), currentString: String,  node: Node, ans: mutable.Set[String]): Unit = {
      val (row, col) = coord
      val char = board(row)(col)
      node(char) match {
        case Some(nextNode) =>
            val newString = currentString + char
            if(nextNode.isWord) ans += newString
            board(row)(col) = '#'
            neighbors(coord, (board.length, board(0).length)).foreach {
              case (nr, nc) if board(nr)(nc) != '#' => _dfs((nr, nc), newString, nextNode, ans)
              case _ =>
           }
          board(row)(col) = char
          /** pruning */
          if(nextNode.next.isEmpty) node.next.remove(char)

        case None =>
      }
    }
    val ans = mutable.Set[String]()
    for(i <- board.indices; j <- board(0).indices) {
      _dfs((i, j), "", tries.root, ans)
    }
    ans.toList
  }
  private val neighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    Seq(
      (row + 1, col),
      (row - 1, col),
      (row, col + 1),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 && 0 <= c && c < shape._2}
  }
}


/**
* tries + dfs + pruning
* memo
*   1. a seenBoard to record which position was visited 
* time complexity: 
* 
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


/**
* implement prefix tries by hashmap
* memo
*   1. in dfs, we directly input the node from tries instead of tries itself 
*   2. pruning an edge after matching a word and its children couldn't represent a word
*   3. this solution is faster than solution1
*/

import scala.collection.mutable
case class Node(next: mutable.Map[Char, Node] = mutable.Map.empty, var isWord: Boolean = false){
  def apply(char: Char): Option[Node] = next.get(char)
  def update(char: Char, node: Node): Unit = next(char) = node
}

class Tries(){
  val root = Node()
  def insert(word: String): Unit = {
    var node = root
    word.foreach { c =>
      node(c) match {
        case Some(n) => node = n
        case None =>
          node(c) = Node()
          node = node(c).get
      }
    }
    node.isWord = true
  }

  def startsWith(prefix: String): Boolean = searchUtil(prefix).isDefined
  def search(word: String): Boolean =  searchUtil(word).exists(_.isWord)

  def searchUtil(s: String): Option[Node] = {
    var node = root
    s.foreach { c =>
      node(c) match {
        case Some(n) => node = n
        case None => return None
      }
    }
    Some(node)
  }
}


object Solution2 {
  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val tries = new Tries()
    words.foreach(tries.insert)
    dfs(tries, board)
  }

  def dfs(tries: Tries, board: Array[Array[Char]]): List[String] = {
    def _dfs(coord: (Int, Int), currentString: String,  node: Node, ans: mutable.Set[String]): Unit = {
      val (row, col) = coord
      val char = board(row)(col)
      node(char) match {
        case Some(nextNode) =>
            val newString = currentString + char
            if(nextNode.isWord) ans += newString
            board(row)(col) = '#'
            neighbors(coord, (board.length, board(0).length)).foreach {
              case (nr, nc) if board(nr)(nc) != '#' => _dfs((nr, nc), newString, nextNode, ans)
              case _ =>
           }
          board(row)(col) = char
          /** pruning */
          if(nextNode.next.isEmpty) node.next.remove(char)

        case None =>
      }
    }
    val ans = mutable.Set[String]()
    for(i <- board.indices; j <- board(0).indices) {
      _dfs((i, j), "", tries.root, ans)
    }
    ans.toList
  }
  private val neighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    Seq(
      (row + 1, col),
      (row - 1, col),
      (row, col + 1),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 && 0 <= c && c < shape._2}
  }
}




