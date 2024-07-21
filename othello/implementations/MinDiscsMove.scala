package othello.implementations

import othello.{Algorithm, Bitboard, BoardState, count}

class MinDiscsMove extends Algorithm {
  override val name = "MinDiscsMove"
  override def findBestMove(game: BoardState): Bitboard = game.getMoves.minBy { move =>
    val player = game.currentPlayer
    game.board.playMove(player, move).getBoard(player).count
  }
}
