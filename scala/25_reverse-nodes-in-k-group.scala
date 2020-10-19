/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
 
 /**
 * my first commit
 * time complexity: O(N)
 * space complexity: O(1)
 */
object Solution1 {
  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    if(head == null) return head

    val newHead = new ListNode(0, head)
    _reverseKGroup(newHead, k)

    newHead.next
  }
  def _reverseKGroup(preNode: ListNode, k: Int ): Unit = {


    if(checkSubListValid(preNode.next, k)){
    /**
    *                                    head     tail
    * reverse subList ABCD from preNode->A->B->C->D->tailNext to preNode->D->C->B->A->tailNext
    *
    */
      var tailNext = preNode.next
      val head = preNode.next
      (1 to k).foreach(_ => tailNext = tailNext.next)


      preNode.next = reverseLinkedList(head, k)  // return reversed-sublist's new head
      head.next = tailNext
      _reverseKGroup(head, k)

    }
  }

  private def reverseLinkedList(head: ListNode, k: Int): ListNode = {
    var pre: ListNode = null
    var curr = head

    for(_ <- 1 to k) {
      val tmp = curr.next
      curr.next = pre
      pre = curr
      curr = tmp
    }
    pre
  }

  private def checkSubListValid(current: ListNode, k: Int): Boolean = {

    var node = current
    for(i <- 1 to k) {
      if(node == null) return false
      node = node.next
    }
    true

  }
}