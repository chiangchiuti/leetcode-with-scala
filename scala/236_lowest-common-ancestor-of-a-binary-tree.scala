/**
 * Definition for a binary tree node.
 * class TreeNode(var _value: Int) {
 *   var value: Int = _value
 *   var left: TreeNode = null
 *   var right: TreeNode = null
 * }
 */

/**
*  select solution
*  DFS with recursive
*  time complexity O(N)
*  space complexity O(N)
*/
object Solution0 {
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    _lowestCommonAncestor(root, p, q)
  }

  private def _lowestCommonAncestor(node: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    if (node == null || node == p || node == q) return node
    /**
    *  1. if p and q are node 's child, return p q 's LCA 
    *  2.  if p and q are not node's child return null
    *  3. if p and q, only one of then ar node's child return that node (p or q)
    */
    val left = _lowestCommonAncestor(node.left, p, q)
    val right = _lowestCommonAncestor(node.right, p, q)

    (left, right) match {
      case (null, _) => right  // p and q are both not in left
      case (_, null) => left  // p and q are both not in right
      case (l, r) =>  node // only lowest common ancestor could return both non null node
      // p and q, one of then in left and the other one in right
    }
  }
}