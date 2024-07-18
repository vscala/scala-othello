import othello.{Algorithm, Simulator}
import othello.implementations.{HighIndexMove, LowIndexMove, MaxDiscsMove, MinDiscsMove}

@main
def main(): Unit = {
  val highIndexMove = HighIndexMove()
  val lowIndexMove = LowIndexMove()
  val maxDiscsMove = MaxDiscsMove()
  val minDiscsMove = MinDiscsMove()

  val algorithms: List[Algorithm] = List(highIndexMove, lowIndexMove, maxDiscsMove, minDiscsMove)

  for {
    algorithm1 <- algorithms
    algorithm2 <- algorithms
  } {
    val start = System.currentTimeMillis()
    val result = Simulator.simulate(algorithm1, algorithm2)
    val elapsed = System.currentTimeMillis() - start
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
    if (result == "Tie") println(s"$winner tied $loser playing as $result in $elapsed ms")
    else println(s"$winner won against $loser playing as $result in $elapsed ms")
  }
}