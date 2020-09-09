class MyQueue() {

    /** Initialize your data structure here. */
    
    private val stack1 = scala.collection.mutable.Stack[Int]()
    private val stack2 = scala.collection.mutable.Stack[Int]()

    /** Push element x to the back of queue. */
    def push(x: Int) {
        stack1.push(x)
    }
    
    private def mv() {
        if(stack2.isEmpty)
            while(!stack1.isEmpty)
                stack2.push(stack1.pop)
    }

    /** Removes the element from in front of queue and returns that element. */
    def pop(): Int = {
        mv()
        if(stack2.isEmpty) -1 else stack2.pop  
       
   
    }

    /** Get the front element. */
    def peek(): Int = {
        mv()
        if(stack2.isEmpty) -1 else stack2.head  
        
    }


    /** Returns whether the queue is empty. */
    def empty(): Boolean = {
        stack1.isEmpty && stack2.isEmpty
    }

}

/**
 * Your MyQueue object will be instantiated and called as such:
 * var obj = new MyQueue()
 * obj.push(x)
 * var param_2 = obj.pop()
 * var param_3 = obj.peek()
 * var param_4 = obj.empty()
 */