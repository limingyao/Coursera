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
    if(c==r){
      1
    } else if (c==0){
      1
    } else {
      pascal(c,r-1)+pascal(c-1,r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val list = chars.filter(x => x=='('|| x==')')
    remove(list)
  }
  def remove(chars: List[Char]):Boolean = {
    if(chars.isEmpty){
      true
    } else if(chars.size % 2 != 0 ) {
      false
    } else if( chars(0)=='(' && chars(1)==')' ) {
      remove(chars.drop(2))
    } else if(chars(chars.size-2)=='(' && chars(chars.size-1)==')') {
      remove(chars.slice(0,chars.size-2))
    } else if(chars(0)=='(' && chars(chars.size-1)==')') {
      remove(chars.slice(1,chars.size-1)) 
    } else {
      false
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (coins.size == 0 || money < 0)
      0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
