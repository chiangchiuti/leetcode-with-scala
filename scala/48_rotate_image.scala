/**
* my first commitment
* rotate 4 cell in each iteration
*
*   pattern:  (row, col) -> (col, n - 1- row)
*       1. (i, j) - > (j, n - 1 -i)
*       2. (j, n - 1 -i) -> (n - 1 - i, n - 1 - j)
*       3. (n - 1 - i, n - 1 - j) -> (n -1 -j, n - 1 - (n -1 - i) ) =  (n - 1 -j, i)
*       4. (n - 1 -j, i) -> (i, n - 1 - (n - 1 - j)) = (i, j)
*
* ((0,0) -> (0,3) -> (3,3) -> (3,0))
* ((0,1) -> (1,3) -> (3,2) -> (2,0))
* ((1,0) -> (0,2) -> (2,3) -> (3,1))
* ((1,1) -> (1,2) -> (2,2) -> (2,1))
* 
*/
object Solution1 {
    def rotate(matrix: Array[Array[Int]]): Unit = {
      val n = matrix.size
      printMatrix(n)
      
      for (i <- 0 until (n / 2).toInt + n % 2; j <- 0 until (n / 2).toInt){      
        val tmp = matrix(n - 1 -j)(i)
        matrix(n - 1 - j)(i) = matrix(n - 1 - i)(n - j - 1)
        matrix(n - 1 - i)(n - j - 1) = matrix(j)(n - 1 - i)
        matrix(j)(n - 1 - i) = matrix(i)(j)
        matrix(i)(j) = tmp
      }
    }
    def printMatrix(size: Int): Unit = {
      for (i <- 0 until size) {
        for (j <- 0 until size) {
          print(s"($i, $j) ")
        }
        println(" ")
      }
    }
    /**
        (0, 0) (0, 1) (0, 2) (0, 3)  
        (1, 0) (1, 1) (1, 2) (1, 3)  
        (2, 0) (2, 1) (2, 2) (2, 3)  
        (3, 0) (3, 1) (3, 2) (3, 3)  
    */
}
