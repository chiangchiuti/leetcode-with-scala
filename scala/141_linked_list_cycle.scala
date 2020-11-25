/**
 * Definition for singly-linked list.
 * class ListNode(var _x: Int = 0) {
 *   var next: ListNode = null
 *   var x: Int = _x
 * }
 */

/**
* chosen solution
* memo
*      1. create two pointers one work faster with two step the other work slower with a step
*         if there is a cycle in linked list, the faster pointer will equal to  slower pointer sooner or later
*
* time complexity: 
*       no cycle: O(N)
*       has cycleL O(N + K) K is the cycle length
* space complexity: O(1) )
*/

object Solution0 {
    def hasCycle(head: ListNode): Boolean = {
        if(head != null && head.next != null) 
            _hasCycle(head.next.next, head.next)
        else false
    }
    
    @annotation.tailrec
    def _hasCycle(fast: ListNode, slow: ListNode): Boolean = {
        if(fast == null || fast.next == null || slow == null) return false
        else if(fast == slow) return true
        else _hasCycle(fast.next.next, slow.next)
    }
}


/**
* seen set  iterative version
* memo
*     using a set to record the node which was seen
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution1 {
    def hasCycle(head: ListNode): Boolean = {
        
        var p = head
        val seenSet = new scala.collection.mutable.HashSet[ListNode]()
        
        var result: Boolean = false
        while (p != null && result != true) {

            if(seenSet.contains(p))  
                result = true
            else {
                seenSet += p
                p = p.next
            
            }
        }
        result
    }
}
/**
* seen set - recursive version
* memo
*     using a set to record the node which was seen
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution2 {
    def hasCycle(head: ListNode): Boolean = {
        val seenSet = new scala.collection.mutable.HashSet[ListNode]()
        _hasCycle(head, seenSet)
     
    }
    
    def _hasCycle(n: ListNode, seenSet: scala.collection.mutable.HashSet[ListNode]): Boolean = {
        (n, seenSet.contains(n)) match {
            case (null, _) => false
            case (_, true) => true
            case (_, false) => 
                 seenSet += n
                _hasCycle(n.next, seenSet)
        }
    }
    
}

/**
* two pointer - iterative version
* memo
*      1. create two pointers one work faster with two step the other work slower with a step
*         if there is a cycle in linked list, the faster pointer will equal to  slower pointer sooner or later
*
* time complexity: 
*       no cycle: O(N)
*       has cycleL O(N + K) K is the cycle length
* space complexity: O(1) 
*/
object Solution3 {
    def hasCycle(head: ListNode): Boolean = {
        var pointerA = head
        var pointerB = head
        
        
        var result = false
        while (pointerA != null && pointerA.next != null && result != true) {
            pointerA = pointerA.next.next
            pointerB = pointerB.next
        
            if(pointerA == pointerB) result = true
        }
        result
    }
}

/**
* two pointer - recursive version
* time complexity: 
*       no cycle: O(N)
*       has cycleL O(N + K) K is the cycle length
* space complexity: O(1)     
*/
object Solution4 {
    def hasCycle(head: ListNode): Boolean = {
        if( head != null && head.next != null) 
            _hasCycle(head.next, head)
        else 
            false
    }
    
    def _hasCycle(fast: ListNode, slow: ListNode): Boolean = {
        (fast, slow, fast == slow) match {
            case (null, _, _) => false
            case (_, null, _) => false
            case (_, _, true) => true
            case (_, _, false) => 
                if (fast.next == null) false
                else _hasCycle(fast.next.next, slow.next)   
        }  
    } 
}
/**
* two pointer - tail recursive
*/
object Solution4-1 {
    def hasCycle(head: ListNode): Boolean = {
        if(head != null && head.next != null) 
            _hasCycle(head.next.next, head.next)
        else false
    }
    
    @annotation.tailrec
    def _hasCycle(fast: ListNode, slow: ListNode): Boolean = {
        if(fast == null || fast.next == null || slow == null) return false
        else if(fast == slow) return true
        else _hasCycle(fast.next.next, slow.next)
    }
}
