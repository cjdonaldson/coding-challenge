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
  def opHasPrecedence(p: Precedence, prior: Option[Precedence]) =
    prior match
      case Some(_: Brackets.RightBracketPrecedence) => true
      case Some(pp) => pp.precedence > p.precedence
      case _        => false
