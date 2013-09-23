package recfun
import common._

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

    def aux(c: Int, r: Int) = {
      if (c == 0 || c == r) {
        1;
      }
      else {
        pascal(r - 1, c - 1) + pascal(r - 1, c);
      }
    }

    aux(c,r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    import scala.annotation.tailrec

    @tailrec
    def accumulateParens(chars: List[Char], count: Int): Int = {
      if (count < 0) -1
      else if (chars.isEmpty) count
      else if (chars.head == '(')
          accumulateParens(chars.tail, count + 1)
        else if (chars.head == ')')
        accumulateParens(chars.tail, count - 1)
      else
        accumulateParens(chars.tail, count)
    }

    accumulateParens(chars, 0) == 0;
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      val sortedCoins = coins.sorted
      countChange(money, sortedCoins.init) +
      countChange(money - sortedCoins.max, sortedCoins)
    }
  }
}
