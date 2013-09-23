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

    @tailrec
    def aux(c: Int, r: Int, acum){
      if (c == 0 || c == r) {
        1;
      }
      else {
        pascal(r - 1, c - 1) + pascal(r - 1, c);
      }
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    import scala.annotation.tailrec

    @tailrec
    def aux(chars: List[Char], w: Int): Boolean = {
      if(chars == List.empty) 0 == w

      else {
        if(chars.head == '(')
            aux(chars.tail, w + 1)
          else if(chars.head == ')' && w > 0)
          aux(chars.tail, w - 1)
        else
          aux(chars.tail, w)
      }
    }

    aux(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    1
  }
}
