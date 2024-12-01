package dodropin

// import scala.util.Try

sealed trait Bracket extends OpApplicable:
  val apply: PartialFunction[List[Arg], List[Arg]] =
    case x => x

  val precedence: Int = 6

object Bracket:
  trait Left extends Bracket // with OpApplicable

  trait Right extends Bracket:
    def leftBracket: Left

  object ParenLeft extends Left:
    override def toString: String = "("

  object ParenRight extends Right:
    override val toString: String = ")"

    override val leftBracket: Left = ParenLeft

  object CurlyLeft extends Left:
    override def toString: String = "{"

  object CurlyRight extends Right:
    override val toString: String = "]"

    override val leftBracket: Left = CurlyLeft

  object SquarLeft extends Left:
    override def toString: String = "["

  object SquarRight extends Right:
    override val toString: String = "]"

    override val leftBracket: Left = SquarLeft
