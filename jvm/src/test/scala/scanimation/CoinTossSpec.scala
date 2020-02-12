package scanimation

/** Executes coin toss sequences and calculated the probability to get a number of heads or tails in a row */
object CoinTossSpec extends App {
  def nextBoolean: Boolean = Math.random() < 0.5

  val total = 1000000

  val scores = (0 until total)
    .map { _ =>
      (0 until 100)
        .map(_ => nextBoolean)
        .foldLeft[List[List[Boolean]]](Nil) {
        case (Nil, current) =>
          (current :: Nil) :: Nil
        case (last :: xs, current) if last.head == current =>
          (current :: last) :: xs
        case (seq, current) =>
          (current :: Nil) :: seq
      }
        .map(part => part.size)
        .max
    }
    .groupBy(top => top)
    .toList
    .map { case (key, occurrences) => key -> occurrences.size }

  val highest = scores.map { case (k, v) => k }.max
  (1 to highest).foreach { size =>
    val score = scores.collect { case (k, v) if k >= size => v }.sum
    println(s"$size - $score/$total - ${score / total.toDouble * 100} - ${Math.pow(0.5, size - 1) * 100} - 1/${Math.pow(2, size - 1)}")
  }
}