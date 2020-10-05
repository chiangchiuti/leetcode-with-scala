/**
* DFS traversal 
* time complexity O(N)
*/
object Solution {
    def minDepth(root: TreeNode): Int = {
        if (root == null) return 0
        val left = minDepth(root.left) 
        val right = minDepth(root.right) 
        println(left, right)

        if (left == 0 || right == 0) left + right + 1 else math.min(left, right) + 1
        
    }
}


object Solution {
    def minDepth(root: TreeNode): Int = {
        if(root == null) 0
        else if( root.left == null) minDepth(root.right) + 1
        else if(root.right == null) minDepth(root.left) + 1
        else minDepth(root.right) + 1 min minDepth(root.left) + 1
    
    }
}