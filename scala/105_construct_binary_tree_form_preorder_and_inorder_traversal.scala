
/**
* my first commitment
* memo
*  1. preorder traversal provides us with the placement of the root
*  2. inorder traversal provides us with the placement of the left and right children
* time complexity: O(2N)
*   1. build hashmap O(N)
*   2. build tree O(N)
*/
object Solution1 {
    def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
      val inorderMap = inorder.zipWithIndex.toMap
      buildTree(preorder, 0, inorderMap, 0, inorder.length - 1)
  
    }
  
  
    def buildTree(preorder: Array[Int], preorderIdx: Int, inorderMap: Map[Int, Int], inorderLeft: Int, inorderRight: Int): TreeNode = {
      if (inorderLeft > inorderRight || preorderIdx >= preorder.length) return null

      val currentRootValue = preorder(preorderIdx)
      val node = new TreeNode(currentRootValue)
     
      val preorderIdxOfRight = preorderIdx + inorderMap(currentRootValue) - inorderLeft + 1

      node.left = buildTree(preorder, preorderIdx + 1, inorderMap, inorderLeft, inorderMap(currentRootValue) - 1 )
      
      /**
     * right child's preoder index
     * the problem is how many increment should we have after building the left child tree
     * the answer above is:  the number of node at left child tree when root node is current preorder index  
     *  1. current root index: current preorder index
     *  2. the number nodes of left child tree: 
     *        the number of node between (inorderLeft, inorderMap(currentRootValue)]  in  inorder array
     *
     *  so the child's preorder index is: current preorder index + number of node at left child  + 1 (next)
     *
     */
      
      node.right = buildTree(preorder, preorderIdxOfRight, inorderMap, inorderMap(currentRootValue) + 1, inorderRight)
      
      node
    }
}