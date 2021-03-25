/**
* my first commitment: using extra seen matrix
* memo:
*  1. check next coordination, if have seen it, increase the direction index
* time complexity : O(N)
* space complexity: O(2N): seen matrix + output list
*/
object Solution1 {
    import collection.mutable
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
      val n = matrix.length
      val m = matrix(0).length
      val seen = Array.ofDim[Boolean](n, m)
      val ans = mutable.ListBuffer.empty[Int]
      
      @annotation.tailrec
      def run(directionIdx: Int, coord: (Int, Int), ans: mutable.ListBuffer[Int], targetSize: Int): Unit = {
        if (ans.size == targetSize) return

        val (row, col) = coord
        ans += matrix(row)(col)  
        seen(row)(col) = true


        if (checkNextCoordAvailable(coord, directionIdx, seen)) {
          val direction = getDirection(directionIdx)
          val nextCoord = (row + direction._1, col + direction._2)
          run(directionIdx, nextCoord, ans, targetSize)
        }else {
          val newD = (d + 1) % 4
          val direction = getDirection(newD)
          val nextCoord = (row + direction._1, col + direction._2)
          run(newD, nextCoord, ans, targetSize)
        }

      }

      run(0, (0, 0), ans, n * m)
      ans.toList
    }
  
    
    def checkNextCoordAvailable(coord: (Int, Int), directionIdx: Int, seen: Array[Array[Boolean]]): Boolean = {
      val (row, col) = coord
      val direction = getDirection(directionIdx)
      val nextCoord = (row + direction._1, col + direction._2)

      
      0 <= nextCoord._1 && nextCoord._1 < seen.length && 0 <= nextCoord._2 && nextCoord._2 < seen(0).length && !seen(nextCoord._1)(nextCoord._2)
    }
   
    def getDirection(idx: Int): (Int, Int) = {
      val direction = List (
        (0, 1), // right
        (1, 0), // go down
        (0, -1), // go left
        (-1, 0) // go up
      )
      direction(idx)
    }
}


/**
* counterclockwise rotate matrix
* step:
*  1. add first line to list
*  2. counter-clockwise rotate remaining matrix: transpose + entire reverse
*  
*  remaining:
*  4 5 6
*  7 8 9
* 
* transpose:
*   4 7
*   5 8
*   6 9
* 
* reverse:
*   6 9
*   5 8
*   4 7
*/

object Solution2-1 {
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = { 
        def dfs(mx: Array[Array[Int]]): List[Int] = mx match {
            case mx if mx.isEmpty => List()
            case mx if mx.length == 1 => mx.head.toList
            case _ => mx.head.toList ::: spiralOrder(mx.tail.transpose.reverse)  // counter-clockwise
        }
        dfs(matrix)

    }    
}



/**
* bounded range: 
*  memo:
*    1. direction pattern: right -> down -> left -> up
* time complexity O(N)
* space complexity O(N) : output list
*/
object Solution3-1 {
    import collection.mutable
  
    sealed trait Direction
    case object Right extends Direction
    case object Down extends Direction
    case object Left extends Direction
    case object Up extends Direction
  
    def getNextDirection(direction: Direction): Direction = 
      direction match {
        case Right => Down
        case Down => Left
        case Left => Up
        case Up => Right
      }

  
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
      if (matrix.isEmpty) List.empty
      val n = matrix.length
      val m = matrix(0).length
      val ans = mutable.ListBuffer.empty[Int]
      run(matrix, ans, Right, 0, m - 1, 0, n - 1, n * m)
      ans.toList
    }
  
    def run(matrix: Array[Array[Int]], ans: mutable.ListBuffer[Int], direction: Direction, colLo: Int, colHi: Int, rowLo: Int, rowHi: Int, targetSize: Int): Unit = {
      if (ans.size < targetSize) {
        
        direction match {
          
          case Right => 
          /** 
          * fix rowLo and increase rowLo after traversing right
          */
            (colLo to colHi).foreach(colIdx => ans += matrix(rowLo)(colIdx))
            run(matrix, ans, getNextDirection(direction), colLo, colHi, rowLo + 1, rowHi, targetSize)
          case Down =>
           /** 
          * fix colHi and decrease colHi after traversing down
          */
            (rowLo to rowHi).foreach(rowIdx => ans += matrix(rowIdx)(colHi))
            run(matrix, ans, getNextDirection(direction), colLo, colHi - 1, rowLo, rowHi, targetSize)
          case Left =>
          /** 
          * fix rowHi and decrease rowHi after traversing left
          */
          
            (colHi to colLo by -1).foreach(colIdx => ans += matrix(rowHi)(colIdx))
            run(matrix, ans, getNextDirection(direction), colLo, colHi, rowLo, rowHi - 1, targetSize)
          case Up => 

            /** 
          * fix colLo and increase colLo after traversing up
          */
            (rowHi to rowLo by -1).foreach(rowIdx => ans += matrix(rowIdx)(colLo))
            run(matrix, ans, getNextDirection(direction), colLo + 1, colHi, rowLo, rowHi, targetSize)
          
        }
      }
    }
}