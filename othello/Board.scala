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
    case Direction.North => dilateNorth
    case Direction.South => dilateSouth
    case Direction.West => dilateWest
    case Direction.East => dilateEast
    case Direction.NorthWest => dilateNorthWest
    case Direction.NorthEast => dilateNorthEast
    case Direction.SouthWest => dilateSouthWest
    case Direction.SouthEast => dilateSouthEast
  }

  def and(other: Bitboard): Bitboard = bitboard & other
  def or(other: Bitboard): Bitboard = bitboard | other
  def count: Int = java.lang.Long.bitCount(bitboard)

  def toMoveList: List[Bitboard] = {
    if (bitboard == 0) List.empty
    else (bitboard & -bitboard) :: (bitboard & (bitboard - 1L)).toMoveList
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

case class Board(white: Bitboard, black: Bitboard) {
  @inline
  final def display: String = {
    (0 until 8).map { row =>
      (0 until 8).map { col =>
        val bit = 1L << (row * 8 + col)
        if ((white & bit) != 0) "W"
        else if ((black & bit) != 0) "B"
        else "."
      }.mkString("", " ", "\n")
    }.mkString
  }

  @inline final def occupied: Bitboard = white | black
  @inline final def unoccupied: Bitboard = ~occupied

  @inline
  final def getBoard(boardColor: BoardColor.Value): Bitboard = boardColor match {
    case BoardColor.White => white
    case BoardColor.Black => black
  }

  @inline final def getOtherBoard(boardColor: BoardColor.Value): Bitboard = getBoard(boardColor.other)

  @inline
  final def isValidMove(boardColor: BoardColor.Value)(move: Bitboard): Boolean = {
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

  @inline
  final def filterValidMoves(boardColor: BoardColor.Value, moves: Bitboard): Bitboard = {
    @tailrec
    def checkValidMoves(remain: Bitboard, directions: List[Direction.Value], valid: Bitboard = 0L): Bitboard = {
      if (remain == 0L || directions.isEmpty) valid
      else {
        val direction = directions.head
        val maybeValidForDirection = remain.dilate(direction) & getOtherBoard(boardColor)

        def findValid(remain: Bitboard): Bitboard = {
          if (remain == 0L) 0
          else {
            val current = remain.dilate(direction)
            val foundValid = current & getBoard(boardColor)
            val foundRemain = current & getOtherBoard(boardColor)
            (findValid(foundRemain) | foundValid).dilate(direction.opposite)
          }
        }

        val foundValid = findValid(maybeValidForDirection).dilate(direction.opposite)
        checkValidMoves(remain & ~foundValid, directions.tail, valid | foundValid)
      }
    }
    checkValidMoves(moves, Direction.values.toList)
  }

  extension (bitboard: Bitboard) {
    @inline
    def filterValidMoves(boardColor: BoardColor.Value): Bitboard = Board.this.filterValidMoves(boardColor, bitboard)
  }

  @inline
  final def getMoves(boardColor: BoardColor.Value): List[Bitboard] = {
    getOtherBoard(boardColor)
      .dilate
      .and(unoccupied)
      .filterValidMoves(boardColor)
      .toMoveList
  }

  @inline
  final def playMove(boardColor: BoardColor.Value, move: Bitboard): Board = {
    val bitboard = getBoard(boardColor) | move
    val otherBitboard = getOtherBoard(boardColor)
    val flipped: Bitboard = Direction.values.flatMap { direction =>
      @tailrec
      def flipUntilSwitch(current: Bitboard, lastPos: Bitboard): Option[Bitboard] = {
        if ((lastPos & otherBitboard) != 0)
          flipUntilSwitch(current | current.dilate(direction), lastPos.dilate(direction))
        else Option.when((current & bitboard) != 0)(current)
      }
      Option.when(move.dilate(direction).and(otherBitboard) != 0) {
        flipUntilSwitch(move.dilate(direction), move.dilate(direction))
      }
    }.flatten.foldLeft(bitboard)(_ | _)

    val finalBitboard = bitboard | flipped
    val finalOtherBitboard = otherBitboard & ~flipped

    if (boardColor == BoardColor.White) Board(white = finalBitboard, black = finalOtherBitboard)
    else Board(white = finalOtherBitboard, black = finalBitboard)
  }
}

object Board {
  val start: Board = Board(
    white = 0x0000001008000000L,
    black = 0x0000000810000000L
  )
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
  sealed trait Value {
    def opposite: Value
  }
  case object North extends Value {
    override def opposite: Value = South
  }
  case object South extends Value {
    override def opposite: Value = North
  }
  case object West extends Value {
    override def opposite: Value = East
  }
  case object East extends Value {
    override def opposite: Value = West
  }
  case object NorthWest extends Value {
    override def opposite: Value = SouthEast
  }
  case object NorthEast extends Value {
    override def opposite: Value = SouthWest
  }
  case object SouthWest extends Value {
    override def opposite: Value = NorthEast
  }
  case object SouthEast extends Value {
    override def opposite: Value = NorthWest
  }
  val values: Set[Value] = Set(North, South, West, East, NorthWest, NorthEast, SouthWest, SouthEast)
}

case class BoardState(board: Board, currentPlayer: BoardColor.Value) {
  @inline final def playMove(move: Bitboard): BoardState = BoardState(board.playMove(currentPlayer, move), currentPlayer.other)
  @inline final def skipMove: BoardState = BoardState(board, currentPlayer.other)
  @inline final def getMoves: List[Bitboard] = board.getMoves(currentPlayer)
  @inline final def isGameOver: Boolean = getMoves.isEmpty && skipMove.getMoves.isEmpty
}

object BoardState {
  val start: BoardState = BoardState(Board.start, BoardColor.Black)
}
