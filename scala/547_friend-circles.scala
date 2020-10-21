

/**
* union & find: implement both union by rank and path compression
* time complexity : 
*   in union & find each op:
*           find: very very close to O(1) amortized
*           union: very very close to O(1) amortized
*   the entire ：
*       O(n + M), n is node size, M is the times we call Union operation which cause O(1) 
*       n (constructor) + M (call union times)
*/

object Solution1 {
  def findCircleNum(M: Array[Array[Int]]): Int = {
    val unionFind = new UnionFind(M.length)

    for (i <- M.indices; j <- (i + 1) until M.length; if M(i)(j) == 1) {
      unionFind.union(i, j)
    }
    unionFind.counter

  }
}

class UnionFind(M: Int) {
  val roots = Array.tabulate(M)(i => i)
  val rank = Array.tabulate(M)(i => 1)
  var counter = M

  def findRoot(i: Int): Int = {

    var root = i
    while (root != roots(root)) {
      roots(root) = roots(roots(root))  // path compression
      root = roots(root)
    }
    root
  }

  def connected(a: Int, b: Int): Boolean = {
    findRoot(a) == findRoot(b)
  }

  def union(a: Int, b: Int) {
    val rootA = findRoot(a)
    val rootB = findRoot(b)

    if(rootA == rootB) return

    // union by rank
    if(rank(rootA) > rank(rootB)) {
      roots(rootB) = rootA
    }else if(rank(rootB) > rank(rootA)){
      roots(rootA) = rootB
    }else { // rank equal case
      roots(rootB) = rootA
      rank(rootA) += 1

    }
    counter -= 1
  }
}

/**
* union & find: without counter in union&find to record current cluster
*       O(n + M + n), n is node size, M is the times we call Union operation which cause O(1) 
*       n (construct union & find ) + M (call union times)  + n (n time call findRoot)
*/
object Solution1-2 {
  def findCircleNum(M: Array[Array[Int]]): Int = {
    val unionFind = new UnionFind(M.length)

    for (i <- M.indices; j <- (i + 1) until M.length; if M(i)(j) == 1) {
      unionFind.union(i, j)
    }
    M.indices.map(unionFind.findRoot).distinct.size

  }
}

class UnionFind(M: Int) {
  val roots = Array.tabulate(M)(i => i)
  val rank = Array.tabulate(M)(i => 1)

  def findRoot(i: Int): Int = {

    var root = i
    while (root != roots(root)) {
      roots(root) = roots(roots(root))  // path compression
      root = roots(root)
    }
    root
  }

  def connected(a: Int, b: Int): Boolean = {
    findRoot(a) == findRoot(b)
  }

  def union(a: Int, b: Int) {
    val rootA = findRoot(a)
    val rootB = findRoot(b)

    if(rootA == rootB) return

    // union by rank
    if(rank(rootA) > rank(rootB)) {
      roots(rootB) = rootA
    }else if(rank(rootB) > rank(rootA)){
      roots(rootA) = rootB
    }else { // rank equal case
      roots(rootB) = rootA
      rank(rootA) += 1

    }
  }
}

