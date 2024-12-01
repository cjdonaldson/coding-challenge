package dodropin

/** Returns encoding of PEMDAS / GEMS and functions (named entities).
  *
  * Operation precedence
  *   - 5 Groupings / Parenthesis / brackets: ({[]})
  *   - 4 Functions: sin, cos, tan TODO:
  *   - 3 Exponents
  *   - 2 Multiply | Divide
  *   - 1 Add | Subtract
  */
trait Precedence:
  def precedence: Int

object Precedence:
  trait LeftBracketPrecedence extends Precedence

  trait RightBracketPrecedence extends Precedence:
    def leftBracket: LeftBracketPrecedence

  def opHasPrecedence(right: Precedence, left: Option[Precedence]): Boolean =
    left match
      case Some(_: Bracket.Left) => false
      case Some(left)            => left.precedence > right.precedence
      case _                     => false