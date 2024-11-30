package dodropin

import scala.util.Try

sealed trait Brackets extends OpApplicable:
  val apply: PartialFunction[List[Arg], List[Arg]] =
    case x => x

  val precedence: Int = 6

object Brackets:
  trait LeftBracketPrecedence extends Brackets with OpApplicable

  trait RightBracketPrecedence extends Brackets:
    def leftBracket: LeftBracketPrecedence

  object ParenLeft extends LeftBracketPrecedence:
    override def toString: String = "("

  object ParenRight extends RightBracketPrecedence:
    override val toString: String = ")"
    override val leftBracket: LeftBracketPrecedence = ParenLeft

  object CurlyLeft extends LeftBracketPrecedence:
    override def toString: String = "{"

  object CurlyRight extends RightBracketPrecedence:
    override val toString: String = "]"
    override val leftBracket: LeftBracketPrecedence = CurlyLeft

  object SquarLeft extends LeftBracketPrecedence:
    override def toString: String = "["

  object SquarRight extends RightBracketPrecedence:
    override val toString: String = "]"
    override val leftBracket: LeftBracketPrecedence = SquarLeft
