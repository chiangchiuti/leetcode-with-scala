/**
* select solution
* directly compare char by char
* if there are only one word should be checked in board, brute force is a more efficient method
*/

object Solution0 {
  private val visitedLabel = '#'
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    dfs(word, board)
  }

  def dfs(word: String, board: Array[Array[Char]]): Boolean = {

    def _dfs(coord: (Int, Int), wordIdx: Int): Boolean = {
      val char = board(coord._1)(coord._2)

      if(wordIdx >= word.length || char != word.charAt(wordIdx)) false
      else if(char == word.charAt(wordIdx) && wordIdx == word.length - 1) true
      else {
        board(coord._1)(coord._2) = visitedLabel
        val exists = getNeighbors(coord, (board.length, board(0).length)) exists {
          case (nr, nc) if board(nr)(nc) != visitedLabel => _dfs((nr, nc), wordIdx + 1)
          case _ => false
        }
        board(coord._1)(coord._2) = char
        exists
      }
    }
    
    val coords = for (i <- board.indices.view; j <- board(0).indices.view) yield (i ,j)
    coords.exists(_dfs(_, 0))
  }

  val getNeighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    List(
      (row + 1, col),
      (row - 1, col),
      (row, col + 1),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 && 0 <= c && c < shape._2}
  }
}


/**
*  my first commitment
*    using Tries
*    watch out that uppercase is different from lower case => "POLAND" != "poland"
*/
object Solution1 {
  private val visitedLabel = '#'
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    val tries = new Tries()
    tries.insert(word)
    dfs(tries, board)
  }

  def dfs(tries: Tries, board: Array[Array[Char]]): Boolean = {

    def _dfs(coord: (Int, Int), prePrefix: String): Boolean = {
      val char = board(coord._1)(coord._2)
      val newPrefix = prePrefix + char
      if(tries.search(newPrefix)) true

      else if(tries.startsWith(newPrefix)) {
        board(coord._1)(coord._2) = visitedLabel
        val exists = getNeighbors(coord, (board.length, board(0).length)) exists {
          case (nr, nc) if board(nr)(nc) != visitedLabel => _dfs((nr, nc), newPrefix)
          case _ => false
        }
        board(coord._1)(coord._2) = char
        exists
      } else false
    }
    val coords = for (i <- board.indices.view; j <- board(0).indices.view) yield (i ,j)
    coords.exists(_dfs(_, ""))
  }

  val getNeighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    List(
      (row + 1, col),
      (row - 1, col),
      (row, col + 1),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 && 0 <= c && c < shape._2}
  }
}


/**
* helper class 
*   Tries Node is implemented with Map
*/
case class Node(child: scala.collection.mutable.Map[Char, Node] = scala.collection.mutable.Map.empty[Char, Node], var isWord: Boolean = false) {
  def update(char: Char, node: Node): Unit = child(char) = node
  def apply(char: Char): Option[Node] = child.get(char)
}

class Tries() {
  val root = Node()

  def insert(word: String): Unit = {
    var node = root
    word.foreach{
      case c if node(c).isDefined => node = node(c).get
      case c =>
        node(c) = Node()
        node = node(c).get
    }
    node.isWord = true
  }

  def search(word: String): Boolean = searchUtil(word).exists(_.isWord)

  def startsWith(prefix: String): Boolean = searchUtil(prefix).isDefined

  private def searchUtil(string: String): Option[Node] = {
    var node = root

    string.foreach{
      case c if node(c).isDefined => node = node(c).get
      case _ => return None
    }
    Some(node)
  }
}


/**
* directly compare char by char
* if there are only one word should be checked in board, brute force is a more efficient method
*/

object Solution2 {
  private val visitedLabel = '#'
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    dfs(word, board)
  }

  def dfs(word: String, board: Array[Array[Char]]): Boolean = {

    def _dfs(coord: (Int, Int), wordIdx: Int): Boolean = {
      val char = board(coord._1)(coord._2)

      if(wordIdx >= word.length || char != word.charAt(wordIdx)) false
      else if(char == word.charAt(wordIdx) && wordIdx == word.length - 1) true
      else {
        board(coord._1)(coord._2) = visitedLabel
        val exists = getNeighbors(coord, (board.length, board(0).length)) exists {
          case (nr, nc) if board(nr)(nc) != visitedLabel => _dfs((nr, nc), wordIdx + 1)
          case _ => false
        }
        board(coord._1)(coord._2) = char
        exists
      }
    }
    
    val coords = for (i <- board.indices.view; j <- board(0).indices.view) yield (i ,j)
    coords.exists(_dfs(_, 0))
  }

  val getNeighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    List(
      (row + 1, col),
      (row - 1, col),
      (row, col + 1),
      (row, col - 1)
    ).filter{case (r, c) => 0 <= r && r < shape._1 && 0 <= c && c < shape._2}
  }
}
