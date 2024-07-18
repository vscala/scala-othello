package othello

import scala.annotation.tailrec

type Result = BoardColor.Value | "Tie"
object Simulator {
  def simulate(black: Algorithm, white: Algorithm): Result = {
    @tailrec
    def simulate(state: BoardState = BoardState.start): BoardState = state.currentPlayer match {
      case _ if state.isGameOver =>
        state
      case _ if state.getMoves.isEmpty =>
        simulate(state.skipMove)
      case BoardColor.Black =>
        val move = black.findBestMove(state)
        simulate(state.playMove(move))
      case BoardColor.White =>
        val move = white.findBestMove(state)
        simulate(state.playMove(move))
    }
    val result = simulate()
    val whiteDiff = result.board.white.count - result.board.black.count
    if (whiteDiff > 0) BoardColor.White
    else if (whiteDiff < 0) BoardColor.Black
    else "Tie"
  }
}
