/**
 * Your Trie object will be instantiated and called as such:
 * var obj = new Trie()
 * obj.insert(word)
 * var param_2 = obj.search(word)
 * var param_3 = obj.startsWith(prefix)
 */

/**
* chosen solution
* Node implement by hashmap
*/
case class Node(next: scala.collection.mutable.Map[Char, Node] = scala.collection.mutable.Map(), var isWord: Boolean = false){
  def update(char: Char, node: Node): Unit = next(char) = node
  def apply(char: Char): Option[Node] = next.get(char)
}

class Trie0() {
  /** Initialize your data structure here. */
  val root = Node()

  /** Inserts a word into the trie. */
  def insert(word: String) {
    var node = root
    word.foreach{ c =>
      node(c) match {
        case Some(n) =>
          node = n
        case None =>
          node(c) = Node()
          node = node(c).get
      }
    }
    node.isWord = true
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

    s.foreach{ c =>
      node(c) match {
        case Some(n) => node = n
        case None => return None
      }
    }
    Some(node)
  }

}



/**
* my first commitment
*/
case class Node(childNode: Array[Node] = Array.ofDim[Node](26), var isWord: Boolean = false) {

  def apply(c: Char): Node = {
    this.apply(c.asDigit - 'a'.asDigit)
  }

  def apply(idx: Int): Node = {
    childNode(idx)
  }
}

class Trie1() {

  /** Initialize your data structure here. */
  val root = Node()


  /** Inserts a word into the trie. */
  def insert(word: String) {
    var node = root
    word.foreach { c =>

      val cIdx = c.asDigit - 'a'.asDigit
      if (node.childNode(cIdx) == null) {
        node.childNode(cIdx) = Node()
      }
      node = node(cIdx)
    }
    node.isWord = true

  }

  /** Returns if the word is in the trie. */
  def search(word: String): Boolean = {
    val node = searchUtil(word)

    node != null && node.isWord

  }

  /** Returns if there is any word in the trie that starts with the given prefix. */
  def startsWith(prefix: String): Boolean = {
    searchUtil(prefix) != null
  }


   private def searchUtil(s: String): Node = {
    var node = root
    var continue = true
    for {
      c <- s
      if continue
    } {
      val cIdx = c.asDigit - 'a'.asDigit
      if (node(cIdx) == null) {
        continue = false
      } 
      node = node(cIdx)
    }
    node
  }
}


/**
*  more elegant
*  Node with apply and update
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
class Trie1-2() {

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
      if (node.isWord) {
        result += prefix
      }
      node.childNode.zipWithIndex.foreach {
        case (n, idx) if n != null => _traversal(prefix + ('a' + idx).toChar, n)
        case _ =>
      }

    }

    _traversal("", root)
    result.foreach(s => println(s.mkString("")))

  }

}

/**
* Node implement by hashmap
*/
case class Node(next: scala.collection.mutable.Map[Char, Node] = scala.collection.mutable.Map(), var isWord: Boolean = false){
  def update(char: Char, node: Node): Unit = next(char) = node
  def apply(char: Char): Option[Node] = next.get(char)
}

class Trie2() {
  /** Initialize your data structure here. */
  val root = Node()

  /** Inserts a word into the trie. */
  def insert(word: String) {
    var node = root
    word.foreach{ c =>
      node(c) match {
        case Some(n) =>
          node = n
        case None =>
          node(c) = Node()
          node = node(c).get
      }
    }
    node.isWord = true
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

    s.foreach{ c =>
      node(c) match {
        case Some(n) => node = n
        case None => return None
      }
    }
    Some(node)
  }

}