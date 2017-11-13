package recfun
object Main {
  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==0 && r==0) 1
      else if (c<0) 0
      else if (c>r) 0
      else pascal(c-1,r-1)+ pascal(c,r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkBalance(NumberOpen: Int, chars: List[Char]) : Boolean = {
        if (NumberOpen < 0)
          false
        else if (chars.isEmpty)
          NumberOpen==0
        else if (chars.head == ')')
          checkBalance(NumberOpen-1,chars.tail)
        else if (chars.head == '(')
          checkBalance(NumberOpen+1,chars.tail)
        else
          checkBalance(NumberOpen,chars.tail)
      }
      checkBalance(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money==0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else
      {
        countChange(money-coins.head,coins)+countChange(money,coins.tail)
      }
    }
  }
