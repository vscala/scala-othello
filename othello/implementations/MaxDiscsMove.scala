package othello.implementations

import othello.{Algorithm, Bitboard, BoardState, count}

class MaxDiscsMove extends Algorithm {
  override val name = "MaxDiscsMove"
  override def findBestMove(game: BoardState): Bitboard = game.getMoves.maxBy { move =>
    val player = game.currentPlayer
    game.board.playMove(player, move).getBoard(player).count
  }
}
