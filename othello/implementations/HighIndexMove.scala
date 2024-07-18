package othello.implementations

import othello.{Algorithm, BoardState, Move, getMoves}

class HighIndexMove extends Algorithm {
  override val name = "HighIndexMove"
  override def findBestMove(game: BoardState): Move = game.getMoves.maxBy(_.index)
}
