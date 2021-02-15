package com.bootcamp.adt

import scala.collection.SortedSet

object AlgebraicDataTypes {

  final case class Error(message: String, input: String) {
    override def toString: String = s"Error: $message; input: $input"
  }

  sealed trait Suit
  object Suit {
    final case object Diamond   extends Suit
    final case object Spade     extends Suit
    final case object Club      extends Suit
    final case object Heart     extends Suit

    def from(str: String): Either[Error, Suit] = {
      str match {
        case "h" => Right(Heart)
        case "d" => Right(Diamond)
        case "s" => Right(Spade)
        case "c" => Right(Club)
        case _   => Left(Error("Illegal argument for `Suit`", str))
      }
    }
  }

  sealed trait Rank
  object Rank {
    final case object Two     extends Rank
    final case object Three   extends Rank
    final case object Four    extends Rank
    final case object Five    extends Rank
    final case object Six     extends Rank
    final case object Seven   extends Rank
    final case object Eight   extends Rank
    final case object Nine    extends Rank
    final case object Ten     extends Rank
    final case object Jack    extends Rank
    final case object Queen   extends Rank
    final case object King    extends Rank
    final case object Ace     extends Rank

    def from(str: String): Either[Error, Rank] = {
      str match {
        case "2" => Right(Two)
        case "3" => Right(Three)
        case "4" => Right(Four)
        case "5" => Right(Five)
        case "6" => Right(Six)
        case "7" => Right(Seven)
        case "8" => Right(Eight)
        case "9" => Right(Nine)
        case "T" => Right(Ten)
        case "J" => Right(Jack)
        case "Q" => Right(Queen)
        case "K" => Right(King)
        case "A" => Right(Ace)
        case _   => Left(Error("Illegal argument for `Rank`", str))
      }
    }
  }

  sealed abstract case class Card private (rank: Rank, suit: Suit)
  object Card {
    private val cardLength = 2

    def from(str: String): Either[Error, Card] = {
      if (str.length == cardLength) {
        for {
          rank <- Rank.from(str.take(1))
          suit <- Suit.from(str.takeRight(1))

          card = new Card(rank, suit){}
        } yield card
      }
      else Left(Error("Illegal argument for `Card`", str))
    }
  }

  sealed trait Hand
  object Hand {
    final case class OmahaHand(cards: Set[Card]) extends Hand
    final case class TexasHand(cards: Set[Card]) extends Hand
    final case class FiveCardDrawHand(cards: Set[Card]) extends Hand
  }

  final case class Board private(cards: List[Card]) extends AnyVal
  object Board {
    private val boardLength = 5
    def from(cards: List[Card]): Either[Error, Board] = {
      cards match {
        case board if board.length != boardLength => Left(Error("Invalid board size", "i"))
        case _ => Right(Board(cards))
      }
    }
  }

  sealed trait PokerCombination
  object PokerCombination {
    final case object StraightFlush  extends PokerCombination
    final case object FourOfAKind    extends PokerCombination
    final case object FullHouse      extends PokerCombination
    final case object Flush          extends PokerCombination
    final case object Straight       extends PokerCombination
    final case object ThreeOfAKind   extends PokerCombination
    final case object TwoPair        extends PokerCombination
    final case object Pair           extends PokerCombination
    final case object HighCard       extends PokerCombination
  }

  final case class TestCase(board: Board, hands: Set[Hand])
  final case class TestResult(board: Board, hands: SortedSet[Hand])
}
