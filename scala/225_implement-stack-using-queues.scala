/**
 * Your MyStack object will be instantiated and called as such:
 * var obj = new MyStack()
 * obj.push(x)
 * var param_2 = obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.empty()
 */


/**
* select solution
* one queue version
* time complexity
*   push: O(2n+1) n is the element in queue1
*   pop: O(1)
*   top: O(1)
*/
class MyStack0() {

    /** Initialize your data structure here. */
    val queue1 = scala.collection.mutable.Queue[Int]()


    /** Push element x onto stack. */
    def push(x: Int) {
        val iter = queue1.indices
        queue1.enqueue(x)
        (iter).foreach(e => queue1.enqueue(queue1.dequeue))
        
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       if(queue1.nonEmpty) queue1.dequeue else -1
        
    }

    /** Get the top element. */
    def top(): Int = {
       queue1.headOption.getOrElse(-1)
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty
    }

}




 /**
 * my first commit
 * two queue version
 * time complexity: 
 *   push: O(1)
 *   pop: O(2n - 1)  n is the element in queue1
 *   top: O(2n - 1)
 */
class MyStack1() {

    /** Initialize your data structure here. */
    var queue1 = scala.collection.mutable.Queue[Int]()
    var queue2 = scala.collection.mutable.Queue[Int]()

    /** Push element x onto stack. */
    def push(x: Int) {
        queue1.enqueue(x)
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       while(queue1.size > 1) {
           queue2.enqueue(queue1.dequeue)
       }
    
        val ret = if(queue1.isEmpty) -1 else queue1.dequeue
        val tmp = queue1
        queue1 = queue2
        queue2 = tmp
        ret
        
    }

    /** Get the top element. */
    def top(): Int = {
        while(queue1.size > 1) {
           queue2.enqueue(queue1.dequeue)
        }
        val ret = if(queue1.isEmpty) -1 else queue1.dequeue
        val tmp = queue1
        queue1 = queue2
        queue2 = tmp
        queue1.enqueue(ret)
        ret
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty && queue2.isEmpty
    }

}

/**
* two queue version
* time complexity
*   push: O(2n+1) n is the element in queue1
*   pop: O(1)
*   top: O(1)
*/
class MyStack2() {

    /** Initialize your data structure here. */
    var queue1 = scala.collection.mutable.Queue[Int]()
    var queue2 = scala.collection.mutable.Queue[Int]()

    /** Push element x onto stack. */
    def push(x: Int) {
        queue2.enqueue(x)
        queue2.enqueueAll(queue1.dequeueAll(_ => true))
        
        val tmp = queue1
        queue1 = queue2
        queue2 = tmp
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       if(queue1.nonEmpty) queue1.dequeue else -1
        
    }

    /** Get the top element. */
    def top(): Int = {
       queue1.headOption.getOrElse(-1)
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty && queue2.isEmpty
    }

}

/**
* one queue version
* time complexity
*   push: O(2n+1) n is the element in queue1
*   pop: O(1)
*   top: O(1)
*/
class MyStack3() {

    /** Initialize your data structure here. */
    val queue1 = scala.collection.mutable.Queue[Int]()


    /** Push element x onto stack. */
    def push(x: Int) {
        val iter = queue1.indices
        queue1.enqueue(x)
        (iter).foreach(e => queue1.enqueue(queue1.dequeue))
        
        
    }

    /** Removes the element on top of the stack and returns that element. */
    def pop(): Int = {
       if(queue1.nonEmpty) queue1.dequeue else -1
        
    }

    /** Get the top element. */
    def top(): Int = {
       queue1.headOption.getOrElse(-1)
    }

    /** Returns whether the stack is empty. */
    def empty(): Boolean = {
        queue1.isEmpty
    }

}
