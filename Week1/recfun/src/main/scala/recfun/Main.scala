package recfun

import scala.annotation.tailrec

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
    def pascal(col: Int, row: Int): Int = {
      if(col < 0 || row < 0) 0
      else if(col == 0 || col == row) 1
      else pascal(col-1, row-1) + pascal(col, row-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def balanceIter(chars: List[Char], count: Int): Boolean = {
        if (count < 0)
          false
        else if (chars.isEmpty)
          count == 0
        else if (chars.head == '(')
          balanceIter(chars.tail, count + 1)
        else if (chars.head == ')')
          balanceIter(chars.tail, count - 1)
        else
          balanceIter(chars.tail, count)
      }

      balanceIter(chars, 0)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeIter(currentMoney: Int, currentCoins: List[Int]): Int = {
        if (currentMoney == money) 1
        else if (currentMoney > money || currentCoins.isEmpty) 0
        else {
          countChangeIter(currentMoney + currentCoins.head, currentCoins) + countChangeIter(currentMoney, currentCoins.tail)
        }

      }

      countChangeIter(0, coins)

    }
  }
