/**
* chosen solution
*  using tries:
*    words = ["time", "me", "bell"]
*    reverse word tries: "emit", "em", "lleb"
*  
*  time complexity:
*    build tries : O(M), M is sum up all word's length in words
*    dfs traversal: O(M)
*/

object Solution0 {
  def minimumLengthEncoding(words: Array[String]): Int = {

    val tries = new Tries()
    words.foreach(w => tries.insert(w.reverse))  // build suffix tries which is not the well know suffix tries

    val validWords =  scala.collection.mutable.ListBuffer.empty[String]
    _dfs(tries.root,  "", validWords)
    /** sum(word.length + 1 for word in validWords) */
    validWords.map(_.length + 1).sum  
  }

  private def _dfs(node: Node, currentStr: String, validWords: scala.collection.mutable.ListBuffer[String]): Unit = {
    if(node.child.isEmpty && node.isWord) {
      validWords += currentStr
    }
    node.child.foreach{case (c, n) => _dfs(n, currentStr.+:(c), validWords)}
  }
}
case class Node(child: scala.collection.mutable.Map[Char, Node] = scala.collection.mutable.Map.empty[Char, Node], var isWord: Boolean = false) {
  def apply(char: Char): Option[Node] = child.get(char)
  def update(char: Char, node: Node): Unit = child(char) = node 
}

class Tries() {
    val root = Node()
    def insert(word: String): Unit ={
        var node = root
        word.foreach{
            case c if node(c).isDefined =>  node = node(c).get
            case c => 
                node(c) = Node()
                node = node(c).get
            
        }
        node.isWord = true
    }
    
    def search(str: String): Boolean = searchNode(str).exists(_.isWord)
    def startsWith(prefix: String): Boolean = searchNode(prefix).isDefined
    private def searchNode(str: String): Option[Node] = {
        var node = root
        str.foreach{
            case c if node(c).isDefined => node = node(c).get
            case c => return None
        }
        Some(node)
    }
}
/**
*  my first commitment
*   1. using a Set to collect valid word
*   2. every word in valid word set should not be the other one's suffix
*   time complexity: N(N * N * M)  M is average length of word in words
*   space complexity: O(2N) N is the words length
*/
object Solution1 {
  def minimumLengthEncoding(words: Array[String]): Int = {
    val words2 = words.sortBy(_.length)  // O(NlogN)
    val set = collection.mutable.Set() ++ words2.toSet  // O(N)


    for(i <- words2.indices; j <- i + 1 until words2.length ) {  // O(N(N + 1) / 2) * O(I + 2J)
      if(words2(j) != words2(i) && isSuffixOf(words2(j), words2(i))) set -= words2(i)
    }
    // set.toList.map(_.length + 1).sum
    /** the answer would be sum(word.length + 1 for word in words) */
    set.foldLeft(0){case (ans, word) => ans + word.length + 1}  // O(N)
  }

  val isSuffixOf = (main: String, pattern: String) => main.reverse.startsWith(pattern.reverse)  // O(I) + O(J) + O(J) = O (I + 2J), I is length of main, J is length of pattern

}

/**
* reverse all word in advance
*/
object Solution1-2 {
  def minimumLengthEncoding(words: Array[String]): Int = {
    val sortedReverseWords = words.map(_.reverse)sortBy(_.length)
    val set = collection.mutable.Set() ++ words.toSet
    
    for(i <- sortedReverseWords.indices; j <- i + 1 until sortedReverseWords.length ) {
      if(sortedReverseWords(j) != sortedReverseWords(i) && sortedReverseWords(j).startsWith(sortedReverseWords(i))) set -= sortedReverseWords(i).reverse
    }

    set.foldLeft(0){case (ans, word) => ans + word.length + 1}
  }
}

/**
* without reverse word
*/
object Solution1-3 {
    def minimumLengthEncoding(words: Array[String]): Int = {
       val sortWords =  words.sortBy(_.length)
       val set = collection.mutable.Set() ++ words.toSet
    
        for(i <- sortWords.indices; j <- i + 1 until sortWords.length){
            if(sortWords(j) != sortWords(i) && sortWords(j).endsWith(sortWords(i))) set -= sortWords(i)
        }
        set.foldLeft(0){case (ans, word) => ans + word.length + 1}
    }
}


/**
*  using tries:
*    words = ["time", "me", "bell"]
*    reverse word tries: "emit", "em", "lleb"
*  
*  time complexity:
*    build tries : O(M), M is sum up all word's length in words
*    dfs traversal: O(M)
*/

object Solution2 {
  def minimumLengthEncoding(words: Array[String]): Int = {

    val tries = new Tries()
    words.foreach(w => tries.insert(w.reverse))  // build suffix tries which is not the well know suffix tries

    val validWords =  scala.collection.mutable.ListBuffer.empty[String]
    _dfs(tries.root,  "", validWords)
    /** sum(word.length + 1 for word in validWords) */
    validWords.map(_.length + 1).sum  
  }

  private def _dfs(node: Node, currentStr: String, validWords: scala.collection.mutable.ListBuffer[String]): Unit = {
    if(node.child.isEmpty && node.isWord) {
      validWords += currentStr
    }
    node.child.foreach{case (c, n) => _dfs(n, currentStr.+:(c), validWords)}
  }
}

case class Node(child: scala.collection.mutable.Map[Char, Node] = scala.collection.mutable.Map.empty[Char, Node], var isWord: Boolean = false) {
  def apply(char: Char): Option[Node] = child.get(char)
  def update(char: Char, node: Node): Unit = child(char) = node 
}

class Tries() {
    val root = Node()
    def insert(word: String): Unit ={
        var node = root
        word.foreach{
            case c if node(c).isDefined =>  node = node(c).get
            case c => 
                node(c) = Node()
                node = node(c).get
            
        }
        node.isWord = true
    }
    
    def search(str: String): Boolean = searchNode(str).exists(_.isWord)
    def startsWith(prefix: String): Boolean = searchNode(prefix).isDefined
    private def searchNode(str: String): Option[Node] = {
        var node = root
        str.foreach{
            case c if node(c).isDefined => node = node(c).get
            case c => return None
        }
        Some(node)
    }
}