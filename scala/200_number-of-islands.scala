/**
* chosen solution
* dfs + floodfill
* time complexity: O(N * M) N is the grid length, M is the grid width
*/

object Solution0 {
    private val endLabel = '0'
    def numIslands(grid: Array[Array[Char]]): Int = {
        // val gridReplica = grid.map(_.clone).toArray
        val coords = for (i <- grid.indices; j <- grid(0).indices) yield (i, j)        
        coords.foldLeft(0){case (count, coord) => if(_dfs(grid, coord))  count + 1 else count}
        
    }
    
    def _dfs(grid: Array[Array[Char]], coord: (Int, Int)): Boolean = {
        val (row, col) = coord
        if(grid(row)(col) == endLabel) return false
        
        grid(row)(col) = endLabel
        getValidNeighbors(coord, (grid.length, grid(0).length)).foreach {
            case (nr, nc) if grid(nr)(nc) != endLabel => _dfs(grid, (nr, nc))
            case _ =>
        }
        true
    }
    
    private val getValidNeighbors = (coord: (Int, Int), shape: (Int, Int)) => {
        List(
            (coord._1 + 1, coord._2),
            (coord._1, coord._2 + 1),
            (coord._1 - 1, coord._2),
            (coord._1, coord._2 - 1)
        ).filter{case (row, col) => 0 <= row  && row < shape._1 && 0 <= col && col < shape._2}
    }
}


/**
* my first commit
* dfs + floodfill
* time complexity: O(N * M) N is the grid length, M is the grid width
*/
object Solution1 {
    private val endLabel = '0'
    def numIslands(grid: Array[Array[Char]]): Int = {
        // val gridReplica = grid.map(_.clone).toArray
        val coords = for (i <- grid.indices; j <- grid(0).indices) yield (i, j)        
        coords.foldLeft(0){case (count, coord) => if(_dfs(grid, coord))  count + 1 else count}
        
    }
    
    def _dfs(grid: Array[Array[Char]], coord: (Int, Int)): Boolean = {
        val (row, col) = coord
        if(grid(row)(col) == endLabel) return false
        
        grid(row)(col) = endLabel
        getValidNeighbors(coord, (grid.length, grid(0).length)).foreach {
            case (nr, nc) if grid(nr)(nc) != endLabel => _dfs(grid, (nr, nc))
            case _ =>
        }
        true
    }
    
    private val getValidNeighbors = (coord: (Int, Int), shape: (Int, Int)) => {
        List(
            (coord._1 + 1, coord._2),
            (coord._1, coord._2 + 1),
            (coord._1 - 1, coord._2),
            (coord._1, coord._2 - 1)
        ).filter{case (row, col) => 0 <= row  && row < shape._1 && 0 <= col && col < shape._2}
    }
}

/**
* Union & Find 
* memo
*    1. without modify original grid's elements
* time complexity: O(N * M) both N M is the dimension of grid 
*     both union and find operation's amortized time complexity in UnionFind class are very very close to 1 but not 1
*/

/**
* weighted quick-union with path compression
* all operation's amortized time complexity are very very close to 1
*/
class UnionFind(grid: Array[Array[Char]]) {
  private val n = grid.length
  private val m = grid(0).length
  private val roots = Array.tabulate(n * m){i => i}
  private val rank = Array.fill[Int](n * m)(1)
  var counter = (for(i <- 0 until n; j <- 0 until m ; if grid(i)(j) == '1' ) yield(i, j)).size

  def findRoot(coord: (Int, Int)): Int = {
    var idx = coord._2 + coord._1 * m
    var root = idx

    while(root != roots(root)) {
      root = roots(root)
    }
    /** path compression */
    while(idx != roots(idx)) {
      val tmp = roots(idx)
      roots(idx) = root
      idx = tmp
    }
    root
  }

  def isConnected(coordA: (Int, Int), coordB: (Int, Int)): Boolean = {
    findRoot(coordA) == findRoot(coordB)
  }
  def union(coordA: (Int, Int), coordB: (Int, Int)): Unit = {
    val findA  = findRoot(coordA)
    val findB = findRoot(coordB)
    if(findA == findB) return

    if(rank(findA) > rank(findB)) {
      roots(findB) = findA
    }else if(rank(findA) < rank(findB)) {
      roots(findA) = findB
    }else {
      roots(findA) = findB
      rank(findB) += 1
    }
    counter -= 1
  }

}

object Solution2 {
  private val endLabel = '0'
  def numIslands(grid: Array[Array[Char]]): Int = {
    val unionFind = new UnionFind(grid)
    for(i <- grid.indices; j <- grid(0).indices)
      union((i, j), unionFind, grid)
    unionFind.counter

  }

  def union(coord: (Int, Int), unionFind: UnionFind, grid: Array[Array[Char]]): Unit = {
    val (row, col) = coord
    if(grid(row)(col) == endLabel) return

    neighbors(coord, (grid.length, grid(0).length)).foreach {
      case (nr, nc) if grid(nr)(nc) != endLabel  =>
        unionFind.union(coord, (nr, nc))
      case _ =>
    }
  }

  private val neighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    val (row, col) = coord
    Seq(
      (row + 1, col),
      (row - 1, col),
      (row, col + 1),
      (row, col - 1)
    ).filter{ case (r, c) => 0 <= r && r < shape._1 && 0 <= c && c < shape._2}
  }
}
