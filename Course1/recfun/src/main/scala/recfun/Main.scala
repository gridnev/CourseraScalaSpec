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
    def pascal(c: Int, r: Int): Int  = {
    if (c < 1 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(input: List[Char], stack: String): Boolean = {
        if (input.isEmpty)
          stack.isEmpty
        else if (input.head == '(')
          balance(input.tail, input.head + stack)
        else if (input.head == ')')
          !stack.isEmpty() && balance(input.tail, stack.tail)
        else
          balance(input.tail, stack);
      }

    balance(chars, "")
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeInternal(money: Int, index: Int): Int = {
        if (money < 0 || index < 0) 0
        else if (money == 0 && index == 0) 1
        else
          countChangeInternal(money, index - 1) + countChangeInternal(money - coins(index), index)
      }

    countChangeInternal(money, coins.length - 1)
  }
  }
