package othello

import scala.annotation.tailrec

type Bitboard = Long
object Bitboard {
  val left: Bitboard = 0x0101010101010101L
  val right: Bitboard = left << 7
  val top: Bitboard = 0xFFL
  val bottom: Bitboard = top << 56

  val topLeft: Bitboard = top | left
  val topRight: Bitboard = top | right
  val bottomLeft: Bitboard = bottom | left
  val bottomRight: Bitboard = bottom | right

  val notLeft: Bitboard = ~left
  val notRight: Bitboard = ~right
  val notTop: Bitboard = ~top
  val notBottom: Bitboard = ~bottom

  val notTopLeft: Bitboard = ~topLeft
  val notTopRight: Bitboard = ~topRight
  val notBottomLeft: Bitboard = ~bottomLeft
  val notBottomRight: Bitboard = ~bottomRight
}
extension (bb: Bitboard) {
  def dilateLeft: Bitboard = (bb & Bitboard.notLeft) >>> 1
  def dilateRight: Bitboard = (bb & Bitboard.notRight) << 1
  def dilateUp: Bitboard = (bb & Bitboard.notTop) >>> 8
  def dilateDown: Bitboard = (bb & Bitboard.notBottom) << 8
  def dilateLeftUp: Bitboard = (bb & Bitboard.notTopLeft) >>> 9
  def dilateRightUp: Bitboard = (bb & Bitboard.notTopRight) >>> 7
  def dilateLeftDown: Bitboard = (bb & Bitboard.notBottomLeft) << 7
  def dilateRightDown: Bitboard = (bb & Bitboard.notBottomRight) << 9

  def dilate: Bitboard = {
    dilateLeft | dilateRight | dilateUp | dilateDown | dilateLeftUp | dilateRightUp | dilateLeftDown | dilateRightDown
  }
  def dilate(direction: Direction.Value): Bitboard = direction match {
    case Direction.Up => dilateUp
    case Direction.Down => dilateDown
    case Direction.Left => dilateLeft
    case Direction.Right => dilateRight
    case Direction.UpLeft => dilateLeftUp
    case Direction.UpRight => dilateRightUp
    case Direction.DownLeft => dilateLeftDown
    case Direction.DownRight => dilateRightDown
  }

  def and(other: Bitboard): Bitboard = bb & other
  def or(other: Bitboard): Bitboard = bb | other

  def count: Int = java.lang.Long.bitCount(bb)

  def toMoveSeq: Seq[Move] = {
    (0 until 64).filter { index =>
      (bb & (1L << index)) != 0
    }.map(Move.apply)
  }

  def display: String = {
    val rows = (0 until 8).map { row =>
      val rowBits = 0xFFL << (row * 8)
      val rowStr = (0 until 8).map { col =>
        val bit = 1L << (row * 8 + col)
        if ((bb & bit) != 0) "X" else "."
      }.mkString(" ")
      s"$rowStr\n"
    }
    rows.mkString
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
    val white = board.white
    val black = board.black
    val rows = (0 until 8).map { row =>
      val rowBits = 0xFFL << (row * 8)
      val rowStr = (0 until 8).map { col =>
        val bit = 1L << (row * 8 + col)
        if ((white & bit) != 0) "W"
        else if ((black & bit) != 0) "B"
        else "."
      }.mkString(" ")
      s"$rowStr\n"
    }
    rows.mkString
  }

  def occupied: Bitboard = board.white | board.black
  def unoccupied: Bitboard = ~board.occupied

  def getBoard(boardColor: BoardColor.Value): Bitboard = boardColor match {
    case BoardColor.White => board.white
    case BoardColor.Black => board.black
  }
  def getOtherBoard(boardColor: BoardColor.Value): Bitboard = getBoard(boardColor.other)

  def isValidMove(boardColor: BoardColor.Value)(move: Move): Boolean = {
    isValidMove(boardColor)(move.toBitboard)
  }
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
  case object Up extends Value
  case object Down extends Value
  case object Left extends Value
  case object Right extends Value
  case object UpLeft extends Value
  case object UpRight extends Value
  case object DownLeft extends Value
  case object DownRight extends Value
  val values: Set[Value] = Set(Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight)
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