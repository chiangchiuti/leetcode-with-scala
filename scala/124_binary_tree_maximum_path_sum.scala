/**
* my first commitment: variation of Kadane's algorithm.
*/

object Solution1 {
    def maxPathSum(root: TreeNode): Int = {
        dfs(root)._1
    }
     /**
      * maxEndingHere records the path maximum summation which ending at current node
      * maxSoFar records the maximum sum globally
      */
    def dfs(node: TreeNode): (Int, Int) = {
      if (node == null) return (Int.MinValue, 0)
      
      val (leftSoFar, leftEndingHere) = dfs(node.left)
      val (rightSoFar, rightEndingHere) = dfs(node.right)

      val maxSoFar = leftSoFar max rightSoFar max (node.value + leftEndingHere + rightEndingHere)
      /**
      * we should choose one path witch makes summation maximum ending at current node
      * maxEndingHere is not charge for node.value + leftEndingHere + rightEndingHere
      */
      val maxEndingHere = 0 max (node.value + (leftEndingHere max rightEndingHere))
      (maxSoFar, maxEndingHere)
    }
}