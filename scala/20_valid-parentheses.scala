/**
* my first commitment
* using stack
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution1 {
    def isValid(s: String): Boolean = {
        if(s.isEmpty || s.length % 2 != 0) return false
        val stack = scala.collection.mutable.Stack[Char]()
        
        val mapping = Map('(' -> ')', '{' -> '}', '[' -> ']')

        s.foreach{c => 
            
            if (mapping.contains(c)){
                stack push c
            }else{
                if(stack.isEmpty || mapping(stack.pop) != c) return false 
             
            }
        }
        stack.isEmpty
        
    }
}

/**
* using stack X FP
* time complexity: O(N)
* space complexity: O(N)
*/
object Solution1-2 {
    def isValid(s: String): Boolean = {
        val mapping = Map('(' -> ')', '{' -> '}', '[' -> ']')
        
        s.foldLeft(List.empty[Char]){ (stack, c) => 
            stack match {
                case pop :: stackAfterPop if  c.equals(mapping.getOrElse(pop, None)) => stackAfterPop
                case _ => c +: stack
            }
           
        }.isEmpty
        
    }
}
