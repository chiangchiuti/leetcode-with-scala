/**
* time complexity  O(1)
*/
object Solution {
    def isPowerOfTwo(n: Int): Boolean = {
        n > 0 && (n & (n - 1) ) == 0
    }
}