package othello.implementations

import othello.{Algorithm, BoardState, Move, getMoves}

class LowIndexMove extends Algorithm {
  override val name: String = "LowIndexMove"
  override def findBestMove(game: BoardState): Move = game.getMoves.minBy(_.index)
}
