/**
* select solution
* dfs + floodfill
* time complexity: O(N*M) N is the grid length, M is the grid width
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
* time complexity: O(N*M) N is the grid length, M is the grid width
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
* without modify original grid's elements
*/
class UnionFind(grid: Array[Array[Char]]) {
  private val n = grid.length
  private val m = grid(0).length
  private val roots = Array.tabulate(n * m)(i => i)
  var counter = (for (i <- 0 until n; j <- 0 until m) yield (i, j)).foldLeft(0) { case (c, (i, j)) => if (grid(i)(j) == '1') c + 1 else c }

  private def findRoot(coord: (Int, Int)): Int = {
    var index = coord._1 * m + coord._2
    var root = index

    while (root != roots(root)) root = roots(root)
    // compression
    while (index != roots(index)) {
      val tmp = roots(index)
      roots(index) = root
      index = tmp
    }
    root
  }

  def connected(coordA: (Int, Int), coordB: (Int, Int)): Boolean = {
    findRoot(coordA) == findRoot(coordB)
  }

  def union(coordA: (Int, Int), coordB: (Int, Int)): Unit = {
    val Aroot = findRoot(coordA)
    val Broot = findRoot(coordB)
    if (Aroot != Broot) {
      roots(Aroot) = Broot
      counter -= 1
    }
  }
}


object Solution2 {
  private val endLabel = '0'

  def numIslands(grid: Array[Array[Char]]): Int = {
    val unionFind = new UnionFind(grid)
    val n = grid.length
    val m = grid(0).length


    (for (i <- 0 until n; j <- 0 until m) yield (i, j)).foreach {
      case (row, col) if grid(row)(col) != endLabel => union(unionFind, grid, (row, col), (n, m))
      case _ =>
    }
    unionFind.counter
  }

  private def union(unionFind: UnionFind, grid: Array[Array[Char]], coord: (Int, Int), shape: (Int, Int)): Unit = {
    val (row, col) = coord
    if (grid(row)(col) != endLabel) {
      getValidNeighbors(coord, shape).foreach {
        case (nr, nc) if grid(nr)(nc) != endLabel => unionFind.union(coord, (nr, nc))
        case _ =>
      }
    }

  }


  private val getValidNeighbors = (coord: (Int, Int), shape: (Int, Int)) => {
    Iterator(
      (coord._1 + 1, coord._2),
      (coord._1, coord._2 + 1),
      (coord._1 - 1, coord._2),
      (coord._1, coord._2 - 1)
    ).filter { case (row, col) => 0 <= row && row < shape._1 && 0 <= col && col < shape._2 }
  }
}

