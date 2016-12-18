package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      if( chars.isEmpty ) true

      var open = 0

      def bal( c: Char ): Int = {
        if(c == '(' ) {
          open + 1
        }
        else if( c == ')'  ) {
          open - 1
        }
        else
          open
      }
      def check(c: Char, cs: List[Char]) : Int = {
        if( open < 0 ) open
        else if( cs.isEmpty ) {
          open = bal(c)
          open
        }
        else {
          open = bal(c)
          check( cs.head, cs.tail )
        }
      }
      check(chars.head, chars.tail)
      if(open == 0 ) true else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count( m: Int, c: List[Int]): Int = {
        if( m < 0 || c.isEmpty ) 0
        else if( m == 0 ) 1
        else {
          count(m - c.head, c) + count(m, c.tail)
        }
      }
      count(money, coins)
    }
  }
