
/**
* chosen solution
* DP + bit operation 
* using an array to record previous result, and current one just add 1 with previous calculated result
* complexity:
*   time complexity: O(N)
*   space complexity: O(N)
*/

object Solution0 {
    def countBits(num: Int): Array[Int] = {
        var arr = Array.ofDim[Int](num + 1)
        (1 to num).foreach{ n => 
            arr(n) = arr(n & (n -1 )) + 1
        }
        arr     
    }
    
}

object Solution1 {
    def countBits(num: Int): Array[Int] = {
        (0 to num).map(_counter).toArray
        
    }
    private def _counter(n: Int): Int = {
        var counter = 0
        var nn = n
        
        while(nn != 0) {
            counter += 1
            nn = nn & (nn - 1)
        }
        counter 
    }
}

/**
* DP + bit operation 
* using an array to record previous result, and current one just add 1 with previous calculated result
* complexity:
*   time complexity: O(N)
*   space complexity: O(N)
*/

object Solution2 {
    def countBits(num: Int): Array[Int] = {
        var arr = Array.ofDim[Int](num + 1)
        (1 to num).foreach{ n => 
            arr(n) = arr(n & (n -1 )) + 1
        }
        arr     
    }
    
}