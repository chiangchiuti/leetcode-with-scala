/**
* select solution 
* two pointer: tail recursive
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution0 {
    def detectCycle(head: ListNode): ListNode = {
        if(head == null || head.next == null || head.next.next == null) return null
        val meetNode =  _findMeetNode(head.next, head.next.next)
        
        meetNode match {
            case null => null
            case _ => _findStartNode(head, meetNode)
        }
    }
    
    @annotation.tailrec
    def _findStartNode(nodeA: ListNode, nodeB: ListNode): ListNode = {
       if(nodeA == nodeB) nodeA
        else _findStartNode(nodeA.next, nodeB.next)
        
    }
    
    @annotation.tailrec
    def _findMeetNode(slow: ListNode, fast: ListNode): ListNode = {
        if(fast == null || fast.next == null) return null
        if(slow == fast) return slow
        _findMeetNode(slow.next, fast.next.next)
        
    }
}

/**
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution1 {
    def detectCycle(head: ListNode): ListNode = {
        val seenSet = new scala.collection.mutable.HashSet[ListNode]()
        var p = head
        
        var result: ListNode = null

        while (p != null && result == null) {
            // println(result)
            if(seenSet.contains(p))  
                result = p

            else {
                seenSet += p
                p = p.next
            
            }
 
        }
        result
        
    }
}


/**
* two pointer
* without using extra space
*/
object Solution2 {
    def detectCycle(head: ListNode): ListNode = {
        val meetPoint = if (head != null && head.next != null)
            _detectCycle(head.next.next, head.next)
        else None
            
        meetPoint match {
            
            case None => null
            case Some(slow1) => getStartOfLoop(head, meetPoint)   
        }
        
    }
    
    def getStartOfLoop(slow1: ListNode, slow2: ListNode): ListNode = {
        
        if (slow1 != slow2) 
            getStartOfLoop(slow1.next, slow2.next)
        else
            slow2

    }
    
    def _detectCycle(fast: ListNode, slow: ListNode): Option[ListNode] = {
        
        (fast, slow, fast == slow) match {
            case (null, _, _) => None
            case (_, null, _) => None
            case (_, _, true) => Some(slow)
            case (_, _, false) => 
                if(fast.next != null) _detectCycle(fast.next.next, slow.next)
                else None
        }
        
    }
}

/**
* two pointer
* iterative version without extra space
*/

object Solution2-1 {
    def detectCycle(head: ListNode): ListNode = {
        val meetNode = _detectCycle(head)
        
        if(meetNode == null) {
            null
        }else {
         findStartPoint(head, meetNode)   
        }
        
    }
    def findStartPoint(head: ListNode, meet: ListNode): ListNode =  {
        var node1 = head
        var node2 = meet
        
        while(node1 != node2) {
            node1 = node1.next
            node2 = node2.next
        }
        node1
    }
    
    def _detectCycle(head: ListNode): ListNode = {
        var fast = head
        var slow = head
        
        var result:ListNode = null
        while(result == null && fast != null && slow != null && fast.next != null) {
            fast = fast.next.next
            slow = slow.next
            if(fast == slow){
                result = slow
            }
        }
        
        result
    }
}

/**
* two pointer: tail recursive
* time complexity: O(N)
* space complexity: O(1)
*/
object Solution2-2 {
    def detectCycle(head: ListNode): ListNode = {
        if(head == null || head.next == null || head.next.next == null) return null
        val meetNode =  _findMeetNode(head.next, head.next.next)
        
        meetNode match {
            case null => null
            case _ => _findStartNode(head, meetNode)
        }
    }
    
    @annotation.tailrec
    def _findStartNode(nodeA: ListNode, nodeB: ListNode): ListNode = {
       if(nodeA == nodeB) nodeA
        else _findStartNode(nodeA.next, nodeB.next)
        
    }
    
    @annotation.tailrec
    def _findMeetNode(slow: ListNode, fast: ListNode): ListNode = {
        if(fast == null || fast.next == null) return null
        if(slow == fast) return slow
        _findMeetNode(slow.next, fast.next.next)
        
    }
}
