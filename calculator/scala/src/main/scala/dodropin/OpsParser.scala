package dodropin

import scala.util.Try

/** Returns encoding of PEMDAS / GEMS and functions (named entities).
  *
  * Operation precedence
  *   - 5 Groupings / Parenthesis / brackets: ({[]})
  *   - 4 Functions: sin, cos, tan TODO:
  *   - 3 Exponents
  *   - 2 Multiply | Divide
  *   - 1 Add | Subtract
  */
object OpsParser:
  private val parseAdd = """(^\s*\+)""".r.unanchored
  private val parseSub = """(^\s*-)""".r.unanchored
  private val parseMul = """(^\s*\*)""".r.unanchored
  private val parseDiv = """(^\s*/)""".r.unanchored

  private val parseExponent = """(^\s*\^)""".r.unanchored

  private val parseFunc = """(^\s*[a-zA-Z]+[0-9]*)""".r.unanchored

  /** string => (Ops, int)
    *
    * Returns if possible (Ops, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("    + 2") // (Ops.Add, 6)
    * }}}
    */
  def tokenize: PartialFunction[String, (OpApplicable, Int)] =
    case parseAdd(m)                            => (Ops.Add, m.length)
    case parseSub(m)                            => (Ops.Sub, m.length)
    case parseMul(m)                            => (Ops.Mul, m.length)
    case parseDiv(m)                            => (Ops.Div, m.length)
    case parseExponent(m)                       => (Ops.Exp, m.length)
    case parseFunc(m) if Ops.supported(m.strip) => (Ops.Func(m.strip), m.length)

  def getOpAction: PartialFunction[OpApplicable, List[Arg] => List[Arg]] =
    case Ops.Add     => Ops.Add.apply
    case Ops.Sub     => Ops.Sub.apply
    case Ops.Mul     => Ops.Mul.apply
    // case Ops.Div     => Ops.Div.apply
    // case Ops.Exp     => Ops.Exp.apply
    // case x: Ops.Func => x.apply

  def opHasPrecedence(op: Precedence, prior: Option[Precedence]) =
    prior match
      case None => false
      case Some(_: Brackets.LeftBracketPrecedence) => false
      case Some(o) => o.precedence > op.precedence
