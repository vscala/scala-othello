package othello.implementations

import othello.*

class MinDiscsMove extends Algorithm {
  override val name = "MinDiscsMove"
  override def findBestMove(game: BoardState): Move = game.getMoves.minBy { move =>
    val player = game.currentPlayer
    game.board.playMove(player, move).getBoard(player).count
  }
}
