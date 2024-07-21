package othello

abstract class Algorithm {
  val name: String
  def findBestMove(game: BoardState): Bitboard
  override def toString: String = name
}
