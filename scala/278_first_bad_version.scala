/* The isBadVersion API is defined in the parent class VersionControl.
      def isBadVersion(version: Int): Boolean = {} */



/**
* my first commitment
* binary search recursive version
* time complexity
*   log(n)
*/
class Solution1 extends VersionControl {
    def firstBadVersion(n: Int): Int = {
        search(1, n)
    }
  
    def search(left: Int, right: Int): Int  = {
      if (left > right) return -1
       /**
      * it's bad version from mid to n, we could keep right side a bad version
      * the we return left side index as left index equals to right index
      */
      if (left == right) return left
      
      val mid = left + (right - left) / 2
      if (isBadVersion(mid))
     
        search(left, mid) 
      else
        search(mid + 1, right)
    }
}
