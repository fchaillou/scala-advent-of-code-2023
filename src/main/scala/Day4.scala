import cats.effect.{IO, IOApp}

import scala.annotation.tailrec

object Day4 extends IOApp.Simple {
  override def run: IO[Unit] = for {
    lines <- readInput("day4-input.txt")
    cards = lines.map(Card.parse)
    totalScore = cards.map(_.points).sum
    _ <- IO.println(s"Part1 is $totalScore")
    countPerCard = processScratchCards(Vector.fill(cards.size)(1), 0)(cards)
    totalNumberOfCards = countPerCard.sum
    _ <- IO.println(s"Part2 is $totalNumberOfCards")
  } yield ()

  @tailrec
  private def processScratchCards(cardsCount: Vector[Int], currentCard: Int)(
      remainingCards: List[Card]
  ): Vector[Int] = remainingCards match {
    case Nil => cardsCount
    case ::(card, next) =>
      val countForCurrentCard = cardsCount(currentCard)
      val cardWins = card.winningCount
      val updatedCounts =
        (currentCard + 1 to currentCard + cardWins).foldLeft(cardsCount)(
          (count, index) =>
            count.updated(index, count(index) + countForCurrentCard)
        )
      processScratchCards(updatedCounts, currentCard + 1)(next)
  }
}

case class Card(id: Int, winningNumbers: Set[Int], cardNumbers: Set[Int]) {
  def winningCount: Int = winningNumbers.intersect(cardNumbers).size
  def points: Int =
    if (winningCount > 0) Math.pow(2d, winningCount - 1).toInt else 0
}

object Card {
  private val regex = "Card +(\\d+):([ \\d]+)\\|([ \\d]+)".r

  def parse(line: String): Card = {
    def parseNumbers(numbersStr: String) =
      numbersStr.trim.split(" +").toSet.map(_.toInt)

    line match {
      case regex(id, winning, card) =>
        Card(id.toInt, parseNumbers(winning), parseNumbers(card))
    }
  }
}
