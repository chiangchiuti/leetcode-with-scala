/**
* BFS time complexity O(N）
*/
object Solution {
  def levelOrder(root: TreeNode): List[List[Int]] = {
    if(root == null) return List[List[Int]]()

    val buffer =  scala.collection.mutable.Queue[TreeNode]()
    val result =  scala.collection.mutable.ListBuffer[List[Int]]()

    buffer.enqueue(root)
    while(buffer.nonEmpty) {
      val currentLevel = scala.collection.mutable.ListBuffer[Int]()

      for (_ <- 0 until buffer.size) {

        val node = buffer.dequeue
        currentLevel.append(node.value)

        if(node.left != null) buffer.enqueue(node.left)
        if(node.right != null) buffer.enqueue(node.right)
      }
      result += currentLevel.toList
    }

    result.toList

  }
}



/**
* using hashmap to store level-list mapping
*/

object Solution2 {
  def levelOrder(root: TreeNode): List[List[Int]] = {
    val oderMap = scala.collection.mutable.Map[Int, List[Int]]()
    mapOrder(root, 1, oderMap)
    oderMap.values.toList

  }


  def mapOrder(node: TreeNode, level: Int, map: scala.collection.mutable.Map[Int, List[Int]]): Unit = {
    if (node != null) {

      val l = map.get(level)
        .map(_ :+ node.value)
        .getOrElse(List(node.value))

      map(level) = l
      mapOrder(node.left, level + 1, map)
      mapOrder(node.right, level + 1, map)

    }

  }
}