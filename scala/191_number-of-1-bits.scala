/**
* chosen solution
* bit operation - recursive version
* time complexity: O(1)
*/
object Solution0{
    // you need treat n as an unsigned value
    def hammingWeight(n: Int): Int = {
        _hammingWeight(n, 0)
    }
    
    @annotation.tailrec
    def _hammingWeight(n: Int, counter: Int): Int = {
        if(n  == 0) counter
        else _hammingWeight(n & (n - 1), counter + 1 )
    }
}

/**
* my first commitment
* time complexity: fixed size: 32 bits, so O(1)
*/
object Solution0 {
    // you need treat n as an unsigned value
    def hammingWeight(n: Int): Int = {

        var mask = 1
        var counter = 0
        for (_ <- 0 to 32) {

            if ((n & mask) != 0) {
                counter += 1
            }
            mask  = mask << 1
        }
        counter
    }
}


/**
* bit operation - iterative version
* memo
*    1. using bit operation :  x = x & (x -1)  to set the last non zero pos to zero
*
*/
object Solution1 {
    // you need treat n as an unsigned value
    def hammingWeight(n: Int): Int = {

        var nn = n
        var counter = 0
        while(nn != 0) {
            counter += 1
            nn = nn & (nn -1)
        }
        
        counter
    }
}

/**
* bit operation - recursive version
*/
object Solution1-2 {
    // you need treat n as an unsigned value
    def hammingWeight(n: Int): Int = {
        _hammingWeight(n, 0)
    }
    
    @annotation.tailrec
    def _hammingWeight(n: Int, counter: Int): Int = {
        if(n  == 0) counter
        else _hammingWeight(n & (n - 1), counter + 1 )
    }
}
