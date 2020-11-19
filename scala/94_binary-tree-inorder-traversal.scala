/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

/**
* select solution
* iterative version
* time complexity: O(N)
* space complexity: O(N)
*/

object Solution0 {
    def inorderTraversal(root: TreeNode): List[Int] = {
      var node = root
      val stack = new collection.mutable.Stack[TreeNode]()
      val result = new collection.mutable.ListBuffer[Int]()

      while(node != null || stack.nonEmpty) {
        while(node != null){
          stack.push(node)
          node = node.left
        }

        node = stack.pop()
        result += node.value
        node = node.right

      }
      result.toList
    }
}


/**
* iterative version
* time complexity: O(N)
* space complexity: O(N)
*/

object Solution1 {
    def inorderTraversal(root: TreeNode): List[Int] = {
      var node = root
      val stack = new collection.mutable.Stack[TreeNode]()
      val result = new collection.mutable.ListBuffer[Int]()

      while(node != null || stack.nonEmpty) {
        while(node != null){
          stack.push(node)
          node = node.left
        }

        node = stack.pop()
        result += node.value
        node = node.right

      }
      result.toList
    }
}

/**
* recursive version
* time complexity: O(N)
* space complexity: O(logN), worst: O(N)
*/
object Solution2 {
    def inorderTraversal(root: TreeNode): List[Int] = {
        _inorderTraversal(root)
    }
    
    def _inorderTraversal(node: TreeNode): List[Int] = {
        if (node == null) Nil
        else
            _inorderTraversal(node.left) ::: List(node.value) ::: _inorderTraversal(node.right)
        
    }
}