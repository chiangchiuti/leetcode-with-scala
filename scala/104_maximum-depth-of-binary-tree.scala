/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

 /**
* DFS traversal 
* time complexity O(N)
*/
object Solution {
    def maxDepth(root: TreeNode): Int = {
        if (root == null) return 0
        
        val left =  1 + maxDepth(root.left)
        val right = 1 + maxDepth(root.right)
        
        math.max(left, right)
        
        
    }
}


/**
*  simpler
*/
object Solution {
    def maxDepth(root: TreeNode): Int = {
        if (root == null) return 0
        math.max(maxDepth(root.left), maxDepth(root.right)) + 1
        
        
    }
}