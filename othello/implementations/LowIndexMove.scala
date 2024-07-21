package othello.implementations

import othello.{Algorithm, Bitboard, BoardState}

class LowIndexMove extends Algorithm {
  override val name: String = "LowIndexMove"
  override def findBestMove(game: BoardState): Bitboard = game.getMoves.min
}
