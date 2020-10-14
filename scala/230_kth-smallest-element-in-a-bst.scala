/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

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