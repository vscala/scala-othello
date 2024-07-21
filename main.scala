import othello.{Algorithm, BoardState, Simulator}
import othello.implementations.{HighIndexMove, LowIndexMove, MaxDiscsMove, MinDiscsMove}

@main
def main(): Unit = {

  val highIndexMove = HighIndexMove()
  val lowIndexMove = LowIndexMove()
  val maxDiscsMove = MaxDiscsMove()
  val minDiscsMove = MinDiscsMove()

  val algorithms: List[Algorithm] = List(highIndexMove, lowIndexMove, maxDiscsMove, minDiscsMove)

  // Simulate dummy games to help improve timing accuracy
  Simulator.simulate(highIndexMove, lowIndexMove)
  Simulator.simulate(maxDiscsMove, minDiscsMove)

  val start = System.currentTimeMillis()
  for {
    _ <- 1 to 1000
    algorithm1 <- algorithms
    algorithm2 <- algorithms
  } {
    val result = Simulator.simulate(algorithm1, algorithm2)
    /*
    val winner = result match {
      case othello.BoardColor.Black => algorithm1.name
      case othello.BoardColor.White => algorithm2.name
      case _ => ""
    }
    val loser = result match {
      case othello.BoardColor.Black => algorithm2.name
      case othello.BoardColor.White => algorithm1.name
      case _ => ""
    }
    if (result == "Tie") println(s"$winner tied $loser playing as $result")
    else println(s"$winner won against $loser playing as $result")

     */
  }
  val elapsed = System.currentTimeMillis() - start
  println(s"Elapsed time: $elapsed ms") // ~3200ms (2100ms with new parallel getMoves implementation)
}