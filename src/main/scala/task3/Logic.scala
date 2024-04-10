package task3

object Logics:
  trait Logic:
    def hasKnight(row: Int, col: Int): Boolean
    def hasPawn(row: Int, col: Int): Boolean
    def hit(row: Int, col: Int): Boolean

  object Logic:
    import PositioningPolicies.PositioningPolicy
    import HitStrategies.{ HitStrategy, HitStatus }

    def apply(size: Int, positioningPolicy: PositioningPolicy, hitStrategy: HitStrategy): Logic =
      new LogicsImpl(size, positioningPolicy, hitStrategy)

    private class LogicsImpl(val size: Int, val positioningPolicy: PositioningPolicy, val hitStrategy: HitStrategy) extends Logic:
      require(size > 1)

      override def hasKnight(row: Int, col: Int): Boolean = positioningPolicy.knightPosition == (row, col)

      override def hasPawn(row: Int, col: Int): Boolean = positioningPolicy.pawnPosition == (row, col)

      override def hit(row: Int, col: Int): Boolean = (row, col) match
        case (0, 0) | (`size`, `size`) => throw new IndexOutOfBoundsException()
        case (x, y) =>
          val status = hitStrategy.hit(
            positioningPolicy.knightPosition,
            positioningPolicy.pawnPosition,
            (x, y),
          )
          if status == HitStatus.NOT_ALLOWED then false
          else { positioningPolicy.moveKnight(row, col) ; status == HitStatus.SUCCESS }

object PositioningPolicies:
  trait PositioningPolicy:
    def knightPosition: (Int, Int)
    def pawnPosition: (Int, Int)
    def moveKnight(row: Int, col: Int): Unit

  object PositioningPolicy:
    def deterministicPositioningPolicy(size: Int, knightPosition: (Int, Int), pawnPosition: (Int, Int)): PositioningPolicy =
      new DeterministicPositioningPolicy(size, knightPosition, pawnPosition)

    def randomPositioningPolicy(size: Int, randomSeed: Option[Long] = Option.empty): PositioningPolicy =
      new RandomPositioningPolicy(size, randomSeed)

    private class DeterministicPositioningPolicy(
      private val size: Int,
      private val initialKnightPosition: (Int, Int),
      override val pawnPosition: (Int, Int)
    ) extends PositioningPolicy:

      private var _knightPosition = { require(isPositionValid(initialKnightPosition)) ; initialKnightPosition }

      private def isPositionValid(pos: (Int, Int)): Boolean = pos match
        case (pawnPosition._1, pawnPosition._2) => false
        case (x, y) => isPositionInsideBoundaries(pos)

      private def isPositionInsideBoundaries(pos: (Int, Int)): Boolean = pos match
        case (x, y) if x > size | y > size => false
        case (x, y) if x < 0 | y < 0 => false
        case _ => true

      override def knightPosition: (Int, Int) = _knightPosition

      private def knightPosition_=(pos: (Int, Int)): Unit = _knightPosition = pos

      override def moveKnight(row: Int, col: Int): Unit =
        _knightPosition = { require(isPositionInsideBoundaries((row, col))) ; (row, col) }

    end DeterministicPositioningPolicy

    private class RandomPositioningPolicy(
      private val size: Int,
      private val seed: Option[Long] = Option.empty,
    ) extends PositioningPolicy:

      import scala.util.Random

      private val rand: Random = if seed.isDefined then Random(seed.get) else Random
      private val _pawnPosition: (Int, Int) = randomEmptyPosition(Option.empty)
      private val _knightPosition: (Int, Int) = randomEmptyPosition(Option(pawnPosition))
      private val positioningPolicy = PositioningPolicy.deterministicPositioningPolicy(size, _knightPosition, _pawnPosition)

      @scala.annotation.tailrec
      private def randomEmptyPosition(pawn: Option[(Int, Int)]): (Int, Int) = pawn match
        case Some(pos) =>
          val knight = (rand.nextInt(size), rand.nextInt(size))
          if knight != pos then knight else randomEmptyPosition(pawn)

        case _ => (rand.nextInt(size), rand.nextInt(size))

      override def knightPosition: (Int, Int) = positioningPolicy.knightPosition

      override def pawnPosition: (Int, Int) =  positioningPolicy.pawnPosition

      override def moveKnight(row: Int, col: Int): Unit = positioningPolicy.moveKnight(row, col)
    end RandomPositioningPolicy

object HitStrategies:
  enum HitStatus:
    case MISS
    case SUCCESS
    case NOT_ALLOWED

  trait HitStrategy:
    def hit(knight: (Int, Int), pawn: (Int, Int), newKnight: (Int, Int)): HitStatus
    def canMove(fromPosition: (Int, Int), toPosition: (Int, Int)): Boolean

  object HitStrategy:
    def standardKnightHitStrategy(): HitStrategy = new KnightHitStrategy()

    private class KnightHitStrategy extends HitStrategy:
      import scala.math.abs

      override def hit(knight: (Int, Int), pawn: (Int, Int), newKnight: (Int, Int)): HitStatus =
        if canMove(knight, newKnight) then
          if pawn == newKnight then HitStatus.SUCCESS else HitStatus.MISS
        else
          HitStatus.NOT_ALLOWED

      override def canMove(fromPosition: (Int, Int), toPosition: (Int, Int)): Boolean = {
        val x = toPosition._1 - fromPosition._1
        val y = toPosition._2 - fromPosition._2
        x != 0 && y != 0  && abs(x) + abs(x) == 3
      }
