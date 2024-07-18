package othello.implementations

import othello.{Algorithm, BoardState, Move, getMoves, getBoard, playMove, count}

class MaxDiscsMove extends Algorithm {
  override val name = "MaxDiscsMove"
  override def findBestMove(game: BoardState): Move = game.getMoves.maxBy { move =>
    val player = game.currentPlayer
    game.board.playMove(player, move).getBoard(player).count
  }
}
