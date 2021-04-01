/**
* chosen solution
* 
* BFS - recursive
* time complexity
*    worst case: O(N), all node was visited
*/
object Solution0 {
    def minDepth(root: TreeNode): Int = {
        _minDepth(if(root == null) List() else List(root), 0)
    }
    
    @annotation.tailrec
    def _minDepth(queue: List[TreeNode], ans: Int): Int = {
        if(queue.isEmpty) ans
        // node has no child 
        else if(queue.exists(t => t.left == null && t.right == null)) ans + 1
        else _minDepth(queue.flatMap(t => List(t.left, t.right)).filter(_ != null), ans + 1)
    }
}


/**
* DFS traversal - recursive
* memo
* 1.edge case: node only have one child
* time complexity O(N)
*/
object Solution1 {
    def minDepth(root: TreeNode): Int = {
        if (root == null) return 0
        val left = minDepth(root.left) 
        val right = minDepth(root.right) 

        if (left == 0 || right == 0) left + right + 1 else math.min(left, right) + 1
        
    }
}

object Solution1_2 {
    def minDepth(root: TreeNode): Int = {
        if(root == null) 0
        else if(root.left == null) minDepth(root.right) + 1
        else if(root.right == null) minDepth(root.left) + 1
        else minDepth(root.right) + 1 min minDepth(root.left) + 1
    
    }
}

/**
* BFS - iterative
* memo
*   1. queue
*   2. shortcut condition
* time complexity
*    worst case: O(N), all node was visited
*    if the tree was unbalance, BFS may be better
*/
object Solution2 {
    def minDepth(root: TreeNode): Int = {
        if(root == null) return 0
        val queue = scala.collection.mutable.Queue[TreeNode]()
        var depth = 0
        var condition = true
        queue.enqueue(root)
        
        while(queue.nonEmpty && condition){
            depth += 1
            for(_ <- 0 until queue.size; if condition){
                val node = queue.dequeue
                if(node.left == null && node.right == null) condition = false
                else {
                    if(node.left != null) queue.enqueue(node.left)
                    if(node.right != null) queue.enqueue(node.right)
                } 
            } 
        }
        depth
        
    }
}  


/**
* BFS - recursive
* time complexity
*    worst case: O(N), all node was visited
*/
object Solution3 {
    def minDepth(root: TreeNode): Int = {
        _minDepth(if(root == null) List() else List(root), 0)
    }
    
    @annotation.tailrec
    def _minDepth(queue: List[TreeNode], ans: Int): Int = {
        if(queue.isEmpty) ans
        else if(queue.exists(t => t.left == null && t.right == null)) ans + 1
        else _minDepth(queue.flatMap(t => List(t.left, t.right)).filter(_ != null), ans + 1)
    }
}