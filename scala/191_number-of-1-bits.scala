/**
* fixed size: 32 bits, so O(1)
*/
object Solution {
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
* using bit operation :  x = x & (x -1)  to set the last non zero pos to zero
*/
object Solution {
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
