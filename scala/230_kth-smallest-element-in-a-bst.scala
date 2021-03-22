/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

object Solution {
    def kthSmallest(root: TreeNode, k: Int): Int = {

      val stack = collection.mutable.Stack[TreeNode]()
      var node = root
      var counter = 0
      var ans = 0
      while ((counter <= k) && (node != null || stack.nonEmpty)) {
        while(node != null) {
          stack push node
          node = node.left
        }
        node = stack.pop
        
        counter += 1
        if (counter == 1 || counter <= k) {
          ans = node.value
        }
        
        node = node.right
        
      }
      ans
    }
}
/**
* my first commit
* inorder iterative template
* time complexity: O(H + K) => H is tree height, H + K = element in stack
*/

object Solution1 {
    def kthSmallest(root: TreeNode, k: Int): Int = {
        
        val stack = collection.mutable.Stack[TreeNode]()
        var node = root
        var counter = 0
        
        
        while(node != null || stack.nonEmpty) {
            while(node != null) {
                stack push node
                node = node.left
            }
            node = stack.pop
            counter += 1
            if(counter == k) return node.value
            else node = node.right
            
        }
        -1
    }
}

/**
* inorder traversal - recursive version
* time complexity: O(H + k)
*/

object Solution2-1 {
    import scala.collection.mutable
    def kthSmallest(root: TreeNode, k: Int): Int = {
        val ret = _kthSmallest(root, k, mutable.ListBuffer.empty)

        ret(k - 1)
    }
    
    def _kthSmallest(node: TreeNode, k:Int, l: mutable.ListBuffer[Int]): mutable.ListBuffer[Int]  = {
       if(node == null) l
       else {
           _kthSmallest(node.left, k, l)
           l += node.value
           if(l.size >= k) l  // shortcut
           else  _kthSmallest(node.right, k, l)  
       }
    }
}


/**
* a brilliant solution - inorder recursive traversal 
* memo:
*   1. using Either, right records numbers of visited node, left record the value when the condition is meet
* time complexity:
*      O(H + K) H is the height of the tree calculated by log(N) approximately
*/
object Solution2-2 {
  def go (node: TreeNode, k: Int) : Either[Int, Int] = {
     val r =for {
      numElementsLeft <- if (node.left == null) Right (0) else go(node.left, k)
      numElementsRight <- if (numElementsLeft + 1 == k) Left(node.value)
      else
        if (node.right == null) Right(0) else go(node.right, k - (numElementsLeft + 1))

    } yield numElementsLeft + numElementsRight + 1
      println(r)
      r

  }

  def kthSmallest(root: TreeNode, k: Int): Int = {
    go(root, k).left.get
  }
}
