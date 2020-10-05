/**
* using bit manipulation
*/
object Solution {
    def totalNQueens(n: Int): Int = {
        var counter = 0
        
        def _totalNQueens(row: Int, col: Int, slash: Int, inverseSlash: Int) {
            if(row == n) counter += 1
            else {
            
                var availableBits = (~(col | slash | inverseSlash)) & ((1 << n) - 1) // 目前空的 bit, 並取反有空位為 1

                
                while(availableBits != 0) {
                    val pos = availableBits & (- availableBits) // 從目前空的 bit pop 出 最低位為 1 的 bit
                    _totalNQueens(row + 1, col | pos, (slash | pos) << 1, (inverseSlash | pos) >> 1)
                
                    availableBits = availableBits & (availableBits - 1)
                }
            }
            
        }
        _totalNQueens(0, 0, 0, 0)
        counter   
    }
}