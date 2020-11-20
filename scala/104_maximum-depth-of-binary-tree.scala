/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

/**
* Select Solution
* BFS - recursive
* time complexity: O(N), N is the total node in tree
* space complexity: O(logN) depending on the depth of tree
*/
object Solution0 {
    def maxDepth(root: TreeNode): Int = {
        _maxDepth(if(root == null) List() else List(root), 0)
    }
    
    @annotation.tailrec
    def _maxDepth(queue: List[TreeNode], ans: Int): Int = {
        if(queue.isEmpty) ans
        else _maxDepth(queue.flatMap(l => List(l.left, l.right)).filter(_ != null), ans + 1)
    }
}

 /**
* my first commitment
* DFS traversal - recursive
* time complexity O(N)
*/
object Solution1 {
    def maxDepth(root: TreeNode): Int = {
        if (root == null) return 0
        /**
        val left =  1 + maxDepth(root.left)
        val right = 1 + maxDepth(root.right)
        math.max(left, right)
        */
        math.max(maxDepth(root.left), maxDepth(root.right)) + 1
    }
}

/**
* BFS - iterative
* memo
*   1. queue: BFS iterative template
* time complexity: O(N) n is node number in tree
* space complexity: O(logN) , depending on the depth oof the tree
*/


object Solution2 {
    def maxDepth(root: TreeNode): Int = {
        if(root == null) return 0
        var depth = 0
        val queue = scala.collection.mutable.Queue[TreeNode]()
        queue.enqueue(root)

        while(queue.nonEmpty) {
            depth += 1
            for(_ <- 0 until queue.size){
                val node = queue.dequeue
                if(node.left != null) queue.enqueue(node.left)
                if(node.right != null) queue.enqueue(node.right)
            }
        }   
        depth
    }
}

/**
* BFS - recursive
* time complexity: O(N), N is the total node in tree
* space complexity: O(logN) depending on the depth of tree
*/

object Solution3 {
    def maxDepth(root: TreeNode): Int = {
        _maxDepth(if(root == null) List() else List(root), 0)
    }
    
    @annotation.tailrec
    def _maxDepth(queue: List[TreeNode], ans: Int): Int = {
        if(queue.isEmpty) ans
        else _maxDepth(queue.flatMap(l => List(l.left, l.right)).filter(_ != null), ans + 1)
    }
}
