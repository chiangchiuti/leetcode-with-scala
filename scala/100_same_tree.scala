
/**
* my first commitment: recursive dfs traversal 
* time complexity: O(N)
*/
object Solution1 {
    def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
        traversal(p, q)
    }
    
    def traversal(p: TreeNode, q: TreeNode): Boolean = {
      (p, q) match {
        case (null, null) => true
        case (null, _) => false
        case (_, null) => false
        case (pp, qq) if pp.value == qq.value =>
        /**
        * we could travel one side and decide if it need to tavel the other side
        */
          traversal(p.left, q.left) && traversal(p.right, q.right)
        case _ => false
      }
    }
}