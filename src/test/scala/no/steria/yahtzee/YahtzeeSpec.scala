package no.steria.yahtzee

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import Yahtzee._

class YahtzeeSpec extends FlatSpec with ShouldMatchers {

  "Simple categories" should "return zero when no matching dice" in {
    Yahtzee.score(List(2,3,4,5,6), Ones) should equal(0)
  }
  
  it should "score Simple categories" in {
    Yahtzee.score(List(1,1,2,1,2), Ones) should equal(3)
    Yahtzee.score(List(1,2,2,1,2), Twos) should equal(6)
    Yahtzee.score(List(1,2,3,3,1), Threes) should equal(6)
    Yahtzee.score(List(1,2,3,4,1), Fours) should equal(4)
    Yahtzee.score(List(1,5,4,5,6), Fives) should equal(10)
    Yahtzee.score(List(1,6,6,6,1), Sixes) should equal(18)
  }
  
  "X of a kind" should "score three of a kind" in {
    Yahtzee.score(List(1,2,2,3,2), ThreeOfAKind) should equal(6)
  }
  
  it should "return zero when no matching combination" in {
    Yahtzee.score(List(1,2,1,3,2), ThreeOfAKind) should equal(0)  
  }
  
  it should "return three times die value when more than three of a kind" in {
    Yahtzee.score(List(3,3,3,3,3), ThreeOfAKind) should equal(9)
  }
  
  it should "find pair" in {
    Yahtzee.score(List(1,2,3,3,4), Pair) should equal(6)
  }
  
  it should "find highest pair" in {
    Yahtzee.score(List(1,1,2,6,6), Pair) should equal(12)
  }
  
  it should "find four of a kind" in {
    Yahtzee.score(List(6,6,6,6,6), FourOfAKind) should equal(4*6)
  }
  
  "Two pairs" should "return zero when no pair" in {
    Yahtzee.score(List(1,2,4,5,6), TwoPairs) should equal(0)
  }
  
  it should "find two pairs" in {
    Yahtzee.score(List(1,1,5,5,2), TwoPairs) should equal(2+10)
  }
  
  "Full house" should "return zero when no full house" in {
    Yahtzee.score(List(5,4,2,1,6), FullHouse) should equal(0)
  }

  it should "find full house" in {
    Yahtzee.score(List(2,2,4,4,4), FullHouse) should equal(2*2+3*4)
  }
  
  it should "return zero when no match" in {
    Yahtzee.score(List(1,1,2,1,3), FullHouse) should equal(0)
    Yahtzee.score(List(1,4,2,1,3), FullHouse) should equal(0)
  }
  
  "Yahtzee" should "return zero when not yahtzee" in {
    Yahtzee.score(List(1,1,1,1,2), YahtzeeCategory) should equal(0)
  }
  
  it should "return 50 if yahtzee" in {
    Yahtzee.score(List(1,1,1,1,1), YahtzeeCategory) should equal(50)
  }

  "Small straight" should "score zero when no match" in {
    Yahtzee.score(List(1,1,1,1,1), SmallStraight) should equal(0)
  }
  
  it should "return sum when match" in {
    Yahtzee.score(List(4,1,2,5,3), SmallStraight) should equal(1+2+3+4+5)
  }
  
  "Large straight" should "score zero when no match" in {
    Yahtzee.score(List(1,2,3,4,5), LargeStraight) should equal(0)
  }

  it should "return sum when match" in {
    Yahtzee.score(List(2,4,3,5,6), LargeStraight) should equal(20)
  }
  
  "Chance" should "return sum of dice" in {
    Yahtzee.score(List(6,6,6,6,6), Chance) should equal(5*6)
    Yahtzee.score(List(6,4,1,3,6), Chance) should equal(6+4+1+3+6)
  }
}

