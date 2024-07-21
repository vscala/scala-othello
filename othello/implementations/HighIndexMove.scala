package othello.implementations

import othello.{Algorithm, Bitboard, BoardState}

class HighIndexMove extends Algorithm {
  override val name = "HighIndexMove"
  override def findBestMove(game: BoardState): Bitboard = game.getMoves.max
}
