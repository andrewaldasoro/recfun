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
        def factorial(n: Int): Int = {
            def loop(acc: Int, n: Int): Int =
                if (n == 0) acc
                else loop(acc * n, n - 1)
            loop(1, n)
        }
        factorial(r)/(factorial(c)*factorial(r-c))
    }
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def closed(opened: List[Char], n: Int): Boolean = {
            if (opened.isEmpty)
                false
            else if (opened.head == ')' && n == 1)
                balance(opened.tail)
            else if (opened.head == ')' && n > 1)
                closed(opened.tail, n - 1)
            else if (opened.head == '(')
                closed(opened.tail, n + 1)
            else
                closed(opened.tail, n)
        }
        
        if (chars.isEmpty)
            true 
        else if (chars.head == '(')
            closed(chars.tail, 1)
        else if (chars.head == ')')
            false
        else 
            balance(chars.tail)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (money < 0 || coins.isEmpty) 0
        else if (money == 0) 1
        else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
}
