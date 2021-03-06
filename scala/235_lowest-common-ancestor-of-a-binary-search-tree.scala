/**
 * Definition for a binary tree node.
 * class TreeNode(var _value: Int) {
 *   var value: Int = _value
 *   var left: TreeNode = null
 *   var right: TreeNode = null
 * }
 */

/**
* DFS 
* 
* exploit binary search three property:  right > parent value > left
* time complexity : O(N)
* space complexity: O(N)
*/
object Solution1 {
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    _lowestCommonAncestor(root, p, q)
  }
  
  @annotation.tailrec
  private def _lowestCommonAncestor(node: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    (p, q) match {
      case (pp, qq) if p.value > node.value && q.value > node.value  && node != null => _lowestCommonAncestor(node.right, pp, qq)
      case (pp, qq) if p.value < node.value && q.value < node.value && node != null => _lowestCommonAncestor(node.left, pp, qq)
      case _ => node
    }
  }
}


/**
* iterative version
*/
object Solution1-2 {
    def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
      if(root == null) return root
      var node = root
      
      var condition = true
      while(condition && node != null){
        if(q.value > node.value && p.value > node.value)
          node = node.right
        else if (node.value > q.value && node.value > p.value)
          node = node.left
        else 
          condition = false
      }
      node
    }
}