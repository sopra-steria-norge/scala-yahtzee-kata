package no.steria.yahtzee

sealed class Category
case class SimpleCategory(targetDie : Int) extends Category
case class XOfAKind(x:Int) extends Category
case class Straight(range:Range) extends Category

object Yahtzee {
  val Ones = SimpleCategory(1)
  val Twos = SimpleCategory(2)
  val Threes = SimpleCategory(3)
  val Fours = SimpleCategory(4)
  val Fives = SimpleCategory(5)
  val Sixes = SimpleCategory(6)
  val SmallStraight = Straight(1 to 5)
  val LargeStraight = Straight(2 to 6)
  val Pair = XOfAKind(2)
  val ThreeOfAKind = XOfAKind(3)
  val FourOfAKind = XOfAKind(4)
  val TwoPairs = new Category
  val FullHouse = new Category
  val Chance = new Category
  val YahtzeeCategory = new Category

  def score(dice:List[Int], category:Category):Int = {
    val frequencies = calculateFrequencies(dice)
    category match {
      case SimpleCategory(targetDie) =>
        frequencies(targetDie) * targetDie
      case Chance =>
        dice.sum
      case XOfAKind(x) =>
        for (die <- 6 to 1 by -1) {
          if (frequencies(die) >= x) return die*x
        }
        0
      case TwoPairs =>
        val pairs = (1 to 6).filter(frequencies(_) >= 2)
        if (pairs.size == 2) pairs(0)*2 + pairs(1)*2 else 0
      case FullHouse =>
        var pair = 0
        var triple = 0
        for (die <- 6 to 1 by -1) {
          if (frequencies(die) == 2) pair = die
          if (frequencies(die) == 3) triple = die
        }
        if (pair != 0 && triple != 0) pair*2 + triple*3 else 0
      case YahtzeeCategory =>
        for (die <- 6 to 1 by -1) {
          if (frequencies(die) >= 5) return 50
        }
        0
      case Straight(range) =>
        for (die <- range) {
          if (frequencies(die) != 1) return 0
        }
        dice.sum
    }
  }
  
  def calculateFrequencies(dice:List[Int]) = {
    (0 to 6).map { die => dice.filter(_ == die).size }
  }
}
