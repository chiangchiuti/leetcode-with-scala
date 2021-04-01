
/**
* my first commitment
* time complexity: O(N * M)
* space complexity: O(N + M)
*/
object Solution1 {
    import collection.mutable
    def setZeroes(matrix: Array[Array[Int]]): Unit = {
      val cols = mutable.Set.empty[Int]
      val rows = mutable.Set.empty[Int]
      
      for (i <- matrix.indices; j <- matrix(i).indices; if matrix(i)(j) == 0) {
        rows += i
        cols += j
      }
      
      rows.foreach(row => matrix(row).indices.foreach(matrix(row)(_) = 0))
      cols.foreach(col => matrix.indices.foreach(matrix(_)(col) = 0))
    }
}



/**
* using first column and row to record cell to be set to zero
* memo:
*  1. we should set first columns and first row in the last, otherwise we cannot distinguish the zero between set by us and originally is
* time complexity: O(NM)
* space complexity: O(1)
*/
object Solution2 {
    import collection.mutable
    def setZeroes(matrix: Array[Array[Int]]): Unit = {
      var rowZero = false
      var colZero = false
      
      /**
      * using first row and first column as flag 
      */
      for (i <- matrix.indices; j <- matrix(i).indices; if matrix(i)(j) == 0) {
        if (i == 0) rowZero = true
        if (j == 0) colZero = true
        matrix(i)(0) = 0
        matrix(0)(j) = 0
      }
    
      /**
      * set one row to zero except first cell
      */
      (1 until matrix.length).foreach {
        case rowIdx if matrix(rowIdx)(0) == 0 => matrix(rowIdx).indices.foreach(matrix(rowIdx)(_) = 0)
        case _ =>
      }
      
      /**
      * set one column to zero except first cell
      */
      (1 until matrix(0).length).foreach {
        case colIdx if matrix(0)(colIdx) == 0 => matrix.indices.foreach(matrix(_)(colIdx) = 0)
        case _ => 
      }
      
      /**
      * set first column and first row to zero if true
      */
      if(rowZero) matrix(0).indices.foreach(matrix(0)(_) = 0)
      if(colZero) matrix.indices.foreach(matrix(_)(0) = 0)
      
    }
}