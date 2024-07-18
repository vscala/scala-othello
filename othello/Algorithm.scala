package othello

abstract class Algorithm {
  val name: String
  def findBestMove(game: BoardState): Move
  override def toString: String = name
}
