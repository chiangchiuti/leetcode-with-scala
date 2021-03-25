

/**
* my first commitment: backtracking + dp - top-down
* memo:
*   1. cache array record which position could jump to destination
*   2. run the loop of jump step size  backward
*/

object Solution1-1 {
    sealed trait Index
    case object Good extends Index 
    case object Bad extends Index
    case object Unknown extends Index
  
    def canJump(nums: Array[Int]): Boolean = {
        val cache = Array.fill[Index](nums.length)(Unknown)
        cache(cache.length-1) = Good
        dfs(nums, 0, cache)

    }
  
    def dfs(nums: Array[Int], pos: Int, cache: Array[Index]): Boolean = {
      if (cache(pos) != Unknown) {
        return cache(pos) == Good
      }
      
      val furthestJump = ((nums.length - 1) - pos) min nums(pos) // don't jump exceed array's length
      val ret = (furthestJump to 1 by -1).collectFirst {  // 1 to  furthestJump would lead to TLE
        case j if dfs(nums, pos + j, cache) =>  true
      }.getOrElse(false)
      
      
      if (ret) cache(pos) = Good else cache(pos) = Bad
      ret
    }
}

/**
* backtracking: DP bottom-up: more simpler
* memo
* 1. solve problem from tail to head
* 2. cache value: true for GOOD position, false for Bad position
* 3. if cache(0) is true, we could jump to last position from position zero
*/

object Solution1-2 {
    def canJump(nums: Array[Int]): Boolean = {
      val cache = Array.ofDim[Boolean](nums.length)
      cache(cache.length - 1) = true
      
      (nums.length - 2 to 0 by -1).foreach { pos =>
        val furthestJump = ((nums.length - 1) - pos) min nums(pos)
        (furthestJump to 1 by -1).collectFirst {
          case step if cache(pos + step) => 
          cache(pos) = true
          cache(pos)
        }.getOrElse(false)
      }
      cache(0)
    }
}


/**
* Greedy - check each position could jump to last good position
* memo:
*  1. solve problem backward
*  2. record last good position which could jump to last position within multi-hop
*  3. check zero position could jump to last position by checking last position equals to zero
* time complexity: O(N)
*/

object Solution2-1 {
    def canJump(nums: Array[Int]): Boolean = {
      var lastPosition = nums.length - 1
      
      (nums.length - 2 to 0 by -1).foreach{ pos =>
        if((nums(pos) +  pos) >= lastPosition) {
          lastPosition = pos
        }
        
      }
      lastPosition == 0
    }
}


/**
* Greedy: check max reach position
* memo
*  1. record max reach position: if current position is larger than max reach position, it means we couldn't jump to current position and it wouldn't be able to jump to last position
* time complexity: O(N)
*/

object Solution3-1 {
    def canJump(nums: Array[Int]): Boolean = {
      var maxReachPos = nums(0)
      nums.indices.forall { pos =>  
          if (pos > maxReachPos) false  
          else {
            maxReachPos = maxReachPos max (pos + nums(pos))
            true
          }
        }        
    }
}
