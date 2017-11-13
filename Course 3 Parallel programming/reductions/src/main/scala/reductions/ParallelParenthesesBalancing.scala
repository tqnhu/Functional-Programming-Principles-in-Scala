package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def isBalance(counts: Int, chars: Array[Char]): Boolean = {
      if (counts<0) false
      else if (chars.isEmpty) counts == 0
      else if (chars.head == '(') isBalance(counts+1, chars.tail)
      else if (chars.head == ')') isBalance(counts-1, chars.tail)
      else isBalance(counts, chars.tail)
    }
    isBalance(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, close: Int, open: Int) : (Int, Int) = {
      if (from < until) {
        chars(from) match {
          case ')' => if (open == 0) traverse(from + 1, until, close + 1, open) //not "open" but "close"
                      else traverse(from + 1, until, close, open - 1)           //else decrease
          case '(' => traverse(from+1, until, close, open + 1) // always increase if "open"
          case _ => traverse(from + 1, until, close, open)
        }
      }
      else (close, open)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if ((until-from)<=threshold) traverse(from, until,0, 0)
      else {
        val (r1, r2) = parallel(reduce(from, (from+until)/2),
                                reduce((from+until)/2, until))
        if ((r1._2 < r2._1)) (r1._1 + r2._1 - r1._2, r2._2)
        else (r1._1, r1._2 - r2._1 + r2._2)
      }
    }

    val res = reduce(0, chars.length)
    if ((res._1 == 0) && (res._2 == 0)) true
    else false
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
