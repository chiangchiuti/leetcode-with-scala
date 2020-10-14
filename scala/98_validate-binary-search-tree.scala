/**
* select solution
* inorder iterative version only keep pre node
* this is also the inorder-iterative-template
* 
* time complexity: O(N)
*/

object Solution {
   def isValidBST(root: TreeNode): Boolean = {
    val stack = new collection.mutable.Stack[TreeNode]()
    var node = root
    var pre: TreeNode = null
    var result = true
    while ((node != null || stack.nonEmpty) && result) {
      while (node != null) {
        stack push node
        node = node.left
      }

      node = stack.pop
      if (pre != null && node.value <= pre.value) result = false
      pre = node
      node = node.right

    }
    result
  }
}

/**
* inorder traversal: recursive version with all element storing
* Time complexity O(NlogN)  there are a distinct and sorted operation
* space complexity O(N)
*/
object Solution1 {
  def isValidBST(root: TreeNode): Boolean = {
    val inorder = traversal(root)
    inorder equals inorder.distinct.sorted // why distinct here? [1, 1] is not a BST because left tree should be smaller than root. 
  }
  def traversal(node: TreeNode): List[Int] = {
    if(node == null){
      List.empty[Int]
    }else {
      // (traversal(node.left) :+ node.value) ::: traversal(node.right) 
      traversal(node.left) ::: List(node.value) ::: traversal(node.right)
    }
  }
}


/**
* inorder iterative version only keep pre node
*   inorder iterative template
* time complexity: O(N)
*/

object Solution1-2 {
   def isValidBST(root: TreeNode): Boolean = {
    val stack = new collection.mutable.Stack[TreeNode]()
    var node = root
    var pre: TreeNode = null
    var result = true
    while ((node != null || stack.nonEmpty) && result) {
      while (node != null) {
        stack push node
        node = node.left
      }

      node = stack.pop
      if (pre != null && node.value <= pre.value) result = false
      pre = node
      node = node.right

    }
    result
  }
}

/**
* inorder recursive version only keep pre node
* time complexity: O(N)
*  ! Not recommend
*/

object Solution1-3 {
    def isValidBST(root: TreeNode): Boolean = {

    var prev: TreeNode = null
    def _isValidBST(node: TreeNode): Boolean = {
      if (node == null) return true
      if (!_isValidBST(node.left)) return false
      if (prev != null && node.value <= prev.value) {
        return false
      }
      prev = node
      _isValidBST(node.right)
    }
    _isValidBST(root)
  }
}

/**
* giving min max range when recursive
* time complexity: O(N)
*/

object Solution2 {
  def isValidBST(root: TreeNode): Boolean = {

    def _isValidBST(node: TreeNode, min: TreeNode, max: TreeNode): Boolean = {

      if(node == null) true
      else {
        if((min != null && node.value <= min.value) || (max != null  && node.value >= max.value)) false
        else {
          _isValidBST(node.lefmt, min, node) && _isValidBST(node.right, node, max)
        }
      }
    }
    _isValidBST(root, null, null)
  }

}