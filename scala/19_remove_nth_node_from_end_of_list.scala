/**
* my first commitment - fast & slow pointer
* time complexity O(N + N / 2)
*/

object Solution1 {
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      val dummyHead = ListNode(0, head)
      var slow = dummyHead
      var fast = dummyHead
      var counter = 0
      
      while(fast != null && fast.next != null) {
        slow = slow.next
        fast = fast.next.next
        counter += 1
      }
      
      val length = if (fast == null) counter * 2 - 1 else counter * 2
      val targetNodeIndex = length - n + 1
      val slowNodeIndex = counter
      // println(length, targetNodeIndex, slowNodeIndex)
      if (counter < targetNodeIndex) {
        removeIdx(slow, slowNodeIndex, targetNodeIndex)
      }else {
        removeIdx(dummyHead, 0, targetNodeIndex)
      }
      dummyHead.next
    }
  
    def removeIdx(node: ListNode, nodeIdx: Int, targetIdx: Int) {
      var nodeT = node
      var nodeIdxV = nodeIdx
      var preNodeindex = targetIdx - 1
     
      while (nodeIdxV < preNodeindex) {
        nodeIdxV += 1
        nodeT = nodeT.next
      }
      var preNode = nodeT
      var nextNode = nodeT.next.next
      preNode.next = nextNode
    }
}


/**
* two pointer fast & slow 
* memo
*   1. keep fast pointer is n + 1 ahead to slow pointer
*   2. if fast == null, slow pointer would points to the  preNode of target removing node
*
*           t 
*   0 1 2 3 4 5
*   s     f
*     s     f
*       s     f
*         s     f
*/
object Solution1-2 {
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      val dummyHead = ListNode(0, head)
      var slow = dummyHead
      var fast = dummyHead
      
      for (i <- 0 until (n + 1) if fast != null) {
        fast = fast.next
      }
      
      while(fast != null) {
        slow = slow.next
        fast = fast.next
      }
      
      slow.next = slow.next.next
      dummyHead.next
    }
  
}

 
        
