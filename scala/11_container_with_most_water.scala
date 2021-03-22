


/**
* brute force not AC
* time complexity: O(n^2)
*/

object Solution1 {
    def maxArea(height: Array[Int]): Int = {
      
      var currentMax = 0

      for(left <- height.indices; right <- (left + 1) until height.length) {
        val limit = height(right) min height(left)
        val width =  (right - left)
        val volume = limit * width

        currentMax = currentMax max volume
      }
      currentMax
    }  
}


/**
* two pointer version
* memo
*  1. fix left side,, the volume is bounded by left side if left side is shorter 
*  2. fix right side. the volume is bounded by right side if right side is shorter
*/

object Solution2 {
    def maxArea(height: Array[Int]): Int = {
      
      var left = 0
      var right = height.length - 1
      var volume = 0
      
      while(left < right) {
        val current = (right - left) * (height(right) min height(left))
        volume = volume max current
        
        if (height(left) < height(right)) // left is shorter
          left += 1
        else // right is shorter
          right -= 1
    }
        
      }
      volume
        
}

/**
* two - pointer version recursive version
*/
object Solution2-1 {
    def maxArea(height: Array[Int]): Int = {
  
     maxArea(height, 0, height.length - 1, 0)
    }
  
    @annotation.tailrec
    def maxArea(height: Array[Int], left: Int, right: Int, maxVolume: Int): Int = {
      if (left >= right)  maxVolume
      else {
        val currentVolume = (right - left) * (height(right) min height(left))
        var newMaxVolume = currentVolume max maxVolume
        
        if (height(right) > height(left)) 
          maxArea(height, left + 1, right, newMaxVolume)
        else
          maxArea(height, left, right - 1, newMaxVolume)
      }
    }
}