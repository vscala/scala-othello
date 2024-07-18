package othello

import scala.annotation.tailrec

type Bitboard = Long
object Bitboard {
  object Mask {
    val NotWest: Bitboard = 0xfefefefefefefefeL
    val NotEast: Bitboard = 0x7f7f7f7f7f7f7f7fL
    val NotNorth: Bitboard = 0xffffffffffffff00L
    val NotSouth: Bitboard = 0x00ffffffffffffffL
    val NotNorthWest: Bitboard = 0xfefefefefefefe00L
    val NotNorthEast: Bitboard = 0x7f7f7f7f7f7f7f00L
    val NotSouthWest: Bitboard = 0x00fefefefefefefeL
    val NotSouthEast: Bitboard = 0x007f7f7f7f7f7f7fL
  }
}

extension (bitboard: Bitboard) {
  def dilateWest: Bitboard = (bitboard & Bitboard.Mask.NotWest) >>> 1
  def dilateEast: Bitboard = (bitboard & Bitboard.Mask.NotEast) << 1
  def dilateNorth: Bitboard = (bitboard & Bitboard.Mask.NotNorth) >>> 8
  def dilateSouth: Bitboard = (bitboard & Bitboard.Mask.NotSouth) << 8
  def dilateNorthWest: Bitboard = (bitboard & Bitboard.Mask.NotNorthWest) >>> 9
  def dilateNorthEast: Bitboard = (bitboard & Bitboard.Mask.NotNorthEast) >>> 7
  def dilateSouthWest: Bitboard = (bitboard & Bitboard.Mask.NotSouthWest) << 7
  def dilateSouthEast: Bitboard = (bitboard & Bitboard.Mask.NotSouthEast) << 9

  def dilate: Bitboard = {
    dilateWest | dilateEast | dilateNorth | dilateSouth | dilateNorthWest | dilateNorthEast | dilateSouthWest | dilateSouthEast
  }
  def dilate(direction: Direction.Value): Bitboard = direction match {
    case Direction.North =>     dilateNorth
    case Direction.South =>     dilateSouth
    case Direction.West =>      dilateWest
    case Direction.East =>      dilateEast
    case Direction.NorthWest => dilateNorthWest
    case Direction.NorthEast => dilateNorthEast
    case Direction.SouthWest => dilateSouthWest
    case Direction.SouthEast => dilateSouthEast
  }

  def and(other: Bitboard): Bitboard = bitboard & other
  def or(other: Bitboard):  Bitboard = bitboard | other

  def count: Int = java.lang.Long.bitCount(bitboard)

  def toMoveSeq: Seq[Move] = {
    (0 until 64).filter { index =>
      (bitboard & (1L << index)) != 0
    }.map(Move.apply)
  }

  def display: String = {
    (0 until 8).map { row =>
      (0 until 8).map { col =>
        val bit = 1L << (row * 8 + col)
        if ((bitboard & bit) != 0) "X" else "."
      }.mkString("", " ", "\n")
    }.mkString
  }
}

case class Board(white: Bitboard, black: Bitboard)

object Board {
  val start: Board = Board(
    white = 0x0000001008000000L,
    black = 0x0000000810000000L
  )
}

extension (board: Board) {
  def display: String = {
    (0 until 8).map { row =>
      (0 until 8).map { col =>
        val bit = 1L << (row * 8 + col)
        if ((board.white & bit) != 0) "W"
        else if ((board.black & bit) != 0) "B"
        else "."
      }.mkString("", " ", "\n")
    }.mkString
  }

  def occupied: Bitboard = board.white | board.black
  def unoccupied: Bitboard = ~board.occupied

  def getBoard(boardColor: BoardColor.Value): Bitboard = boardColor match {
    case BoardColor.White => board.white
    case BoardColor.Black => board.black
  }
  def getOtherBoard(boardColor: BoardColor.Value): Bitboard = getBoard(boardColor.other)

  def isValidMove(boardColor: BoardColor.Value)(move: Move): Boolean = isValidMove(boardColor)(move.toBitboard)
  def isValidMove(boardColor: BoardColor.Value)(move: Bitboard): Boolean = {
    val board = getBoard(boardColor)
    val otherBoard = getOtherBoard(boardColor)

    Direction.values.exists { direction =>
      @tailrec
      def dilateUntilSwitch(bitboard: Bitboard): Boolean = {
        if ((bitboard & otherBoard) != 0) dilateUntilSwitch(bitboard.dilate(direction))
        else (bitboard & board) != 0
      }
      val dilated = move.dilate(direction)
      (dilated & otherBoard) != 0 && dilateUntilSwitch(dilated.dilate(direction))
    }
  }

  def getMoves(boardColor: BoardColor.Value): Seq[Move] = {
    getOtherBoard(boardColor)
      .dilate
      .and(unoccupied)
      .toMoveSeq
      .filter(isValidMove(boardColor))
  }
  def playMove(boardColor: BoardColor.Value, move: Move): Board = {
    val bitboard = getBoard(boardColor) | move.toBitboard
    val otherBitboard = getOtherBoard(boardColor)
    val flipped: Bitboard = Direction.values.flatMap { direction =>
      @tailrec
      def flipUntilSwitch(current: Bitboard, lastPos: Bitboard): Option[Bitboard] = {
        if ((lastPos & otherBitboard) != 0)
          flipUntilSwitch(current | current.dilate(direction), lastPos.dilate(direction))
        else Option.when((current & bitboard) != 0)(current)
      }
      Option.when(move.toBitboard.dilate(direction).and(otherBitboard) != 0) {
        flipUntilSwitch(move.toBitboard.dilate(direction), move.toBitboard.dilate(direction))
      }
    }.flatten.foldLeft(bitboard)(_ | _)

    val finalBitboard = bitboard | flipped
    val finalOtherBitboard = otherBitboard & ~flipped

    if (boardColor == BoardColor.White) Board(white = finalBitboard, black = finalOtherBitboard)
    else Board(white = finalOtherBitboard, black = finalBitboard)
  }
}

object BoardColor {
  sealed trait Value {
    def other: Value
  }
  case object Black extends Value {
    override def other: Value = White
  }
  case object White extends Value {
    override def other: Value = Black
  }
}

object Direction {
  sealed trait Value
  case object North extends Value
  case object South extends Value
  case object West extends Value
  case object East extends Value
  case object NorthWest extends Value
  case object NorthEast extends Value
  case object SouthWest extends Value
  case object SouthEast extends Value
  val values: Set[Value] = Set(North, South, West, East, NorthWest, NorthEast, SouthWest, SouthEast)
}

case class Move(index: Int) {
  require(index >= 0 && index < 64)
}

extension (move: Move) {
  def toBitboard: Bitboard = 1L << move.index
}

case class BoardState(board: Board, currentPlayer: BoardColor.Value)

object BoardState {
  val start: BoardState = BoardState(Board.start, BoardColor.Black)
}

extension (boardState: BoardState) {
  def playMove(move: Move): BoardState = BoardState(boardState.board.playMove(boardState.currentPlayer, move), boardState.currentPlayer.other)
  def skipMove: BoardState = BoardState(boardState.board, boardState.currentPlayer.other)
  def getMoves: Seq[Move] = boardState.board.getMoves(boardState.currentPlayer)
  def isGameOver: Boolean = boardState.getMoves.isEmpty && boardState.skipMove.getMoves.isEmpty
}