object Solution {
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
* without using extra space?
**/
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

