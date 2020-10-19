/**
 * Definition for singly-linked list.
 * class ListNode(var _x: Int = 0) {
 *   var next: ListNode = null
 *   var x: Int = _x
 * }
 */

/**
* select solution
* 利用兩個 pointer 一個走得快 一個走的慢， 若有 cycle : fast pointer 遲早會追上 slow pointer
* time complexity: O(N)
* space complexity: O(1)
*/

object Solution0 {
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
* 用一個 seen set 記錄看過的, iterative version
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
* 用一個 seen set 記錄看過的, recursive version
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
* 利用兩個 pointer 一個走得快 一個走的慢， 若有 cycle : fast pointer 遲早會追上 slow pointer
* time complexity: O(N)
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