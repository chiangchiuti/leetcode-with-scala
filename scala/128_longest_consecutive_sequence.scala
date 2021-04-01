/**
* my first commitment: union & find + sorted array + two pointer
* time complexity: O(NlogN + N) sort + union find
* space complexity: O(3N): sorted array + roots and rank in Union & find
*/

object Solution1 {
    def longestConsecutive(nums: Array[Int]): Int = {
      
      val l = nums.sorted
      val unionFind = new UnionFind(l)
      union(unionFind, l, 0, 0)
  
      unionFind.roots.groupBy(identity).mapValues(_.length).maxByOption(_._2).getOrElse((0, 0))._2

    }
    def union(unionFind: UnionFind, arr: Array[Int], left: Int, right: Int): Unit = {
      if (right == arr.length) return

      if (arr(right) == arr(left)){
        union(unionFind, arr, left, right + 1)
      }else if((arr(right) - arr(left)) == 1) {
        unionFind.union(left, right)
        union(unionFind, arr, right, right)
      } else {
        union(unionFind, arr, right, right + 1)
      }
    }
}

class UnionFind(nums: Array[Int]) {
  val roots = Array.ofDim[Int](nums.length)
  roots.indices.foreach(idx => roots(idx) = idx)
  val rank = Array.fill[Int](nums.length)(1)
  
  def findRoot(input: Int): Int = {
    var root = input
    while(root != roots(root)) {
      roots(root) = roots(roots(root))
      root = roots(root)
    }
    root
  }
  
  def isConnected(A: Int, B: Int): Boolean = {
    findRoot(A) == findRoot(B)
  }
  
  def union(A: Int, B: Int): Unit = {
    val rootA = findRoot(A) 
    val rootB = findRoot(B)
    if (rootA == rootB) return
    
    val rankA = rank(rootA)
    val rankB = rank(rootB)
    
    if(rankA > rankB) {
      roots(rootB) = rootA

    }else if (rankA < rankB) {
      roots(rootA) = rootB
    }else {
      roots(rootB) = rootA
      rank(rootA) += 1
    }
  }
}

/**
* unionFind + hashset
* time complexity: O(N)
* space complexity: O(3N)
*/

object Solution1-2 {
    import collection.mutable
    def longestConsecutive(nums: Array[Int]): Int = {
      
      val map = mutable.Map.empty[Int, Int]
      
      val unionFind = new UnionFind(nums)
      nums.zipWithIndex.foreach { 
        case (value, idx) if map.contains(value) => 
        case (value, idx) =>
          if (map.contains(value + 1))  unionFind.union(idx, map(value + 1))
          if (map.contains(value - 1)) unionFind.union(idx, map(value - 1))
          map.update(value, idx) 
      }
      unionFind.largestComponentSize()
    }
 
}

class UnionFind(nums: Array[Int]) {
  val roots = Array.ofDim[Int](nums.length)
  roots.indices.foreach(idx => roots(idx) = idx)
  val rank = Array.fill[Int](nums.length)(1)
  
  def findRoot(input: Int): Int = {
    
    var root = input
    while(root != roots(root)) {
      roots(root) = roots(roots(root))
      root = roots(root)
    }
    root
  }
  
  def isConnected(A: Int, B: Int): Boolean = {
    findRoot(A) == findRoot(B)
  }
  
  def union(A: Int, B: Int): Unit = {
    val rootA = findRoot(A) 
    val rootB = findRoot(B)
    if (rootA == rootB) return
    
    val rankA = rank(rootA)
    val rankB = rank(rootB)
    
    if(rankA > rankB) {
      roots(rootB) = rootA
    }else if (rankA < rankB) {
      roots(rootA) = rootB
    }else {
      roots(rootB) = rootA
      rank(rootA) += 1
    }
    
  }
  def largestComponentSize(): Int = {
    /**
    * call findroot at each index in order to compress path
    */
    val map = collection.mutable.Map.empty[Int, Int]
    nums.indices.foreach{idx =>
      val root = findRoot(idx)
      map.get(root) match {
        case Some(v) => map.update(root, v + 1)
        case None => map.update(root, 1)
      }
    }
    map.values.maxOption[Int].getOrElse(0)

  }
  
}


/**
* hashset 
* memo
*  1. find the possible start num i and recursively increase i by 1
* time complexity: O(N)
* space complexity: P(N)
*/

object Solution2 {
    def longestConsecutive(nums: Array[Int]): Int = {
      val sets = nums.toSet
      sets.foldLeft(0) { 
       case (longest, v) if !sets.contains(v - 1) => longest max longestConsecutive(sets, v + 1, 1) // find possible start number
       case (longest, v) => longest
      }  
    }
    @annotation.tailrec
    def longestConsecutive(sets: Set[Int], currentValue: Int, cumulative: Int): Int = {
      if (!sets.contains(currentValue)) cumulative
      else longestConsecutive(sets, currentValue + 1, cumulative + 1)
    }
}