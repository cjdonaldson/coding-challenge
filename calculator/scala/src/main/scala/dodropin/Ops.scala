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
trait Ops extends OpApplicable
  // def apply: PartialFunction[List[Arg], List[Arg]]

  // def pattern: String

object Ops:
  trait AddPrecedence extends Ops:
    def precedence: Int = 1

  trait MulPrecedence extends Ops :
    def precedence: Int = 2

  trait ExpPrecedence extends Ops :
    def precedence: Int = 3

  trait FuncPrecedence extends Ops :
    // precedence 0 forces a right handed eval (usage) of an arg
    def precedence: Int = 0

  object Add extends AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x + y) :: rest

    override def toString: String = "+"

  object Sub extends AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x - y) :: rest

    override def toString: String = "-"

  object Mul extends MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x * y) :: rest

    override def toString: String = "*"

  object Div extends MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x / y) :: rest

    override def toString: String = "/"

  object Exp extends ExpPrecedence:
    import scala.math.pow
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest =>
        Arg.AInt(pow(x, y).toInt) :: rest

      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(pow(x, y)) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(pow(x, y)) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(pow(x, y)) :: rest

    override def toString: String = "^"

  def supported(m: String): Boolean =
    Try(Ops.Func(m).apply.isDefinedAt(List(Arg.ADbl(1)))).toOption
      .contains(true)

  case class Func(fn: String) extends FuncPrecedence :
    val apply: PartialFunction[List[Arg], List[Arg]] =
      fn match
        case "sqr" => {
          case Arg.AInt(x) :: rest =>
            Arg.AInt(x * x) :: rest
          case Arg.ADbl(x) :: rest =>
            Arg.ADbl(x * x) :: rest
        }
        case "sqrt" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADbl(scala.math.sqrt(x)) :: rest
          case Arg.ADbl(x) :: rest =>
            Arg.ADbl(scala.math.sqrt(x)) :: rest
        }
        case "sin" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADbl(scala.math.sin(x)) :: rest
          case Arg.ADbl(x) :: rest =>
            Arg.ADbl(scala.math.sin(x)) :: rest
        }
        case "cos" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADbl(scala.math.cos(x)) :: rest
          case Arg.ADbl(x) :: rest =>
            Arg.ADbl(scala.math.cos(x)) :: rest
        }
        case "tan" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADbl(scala.math.tan(x)) :: rest
          case Arg.ADbl(x) :: rest =>
            Arg.ADbl(scala.math.tan(x)) :: rest
        }

    override def toString: String = fn

  // private val parseAdd = """(^\s*\+)""".r.unanchored
  // private val parseSub = """(^\s*-)""".r.unanchored
  // private val parseMul = """(^\s*\*)""".r.unanchored
  // private val parseDiv = """(^\s*/)""".r.unanchored
  //
  // private val parseExponent = """(^\s*\^)""".r.unanchored
  //
  // private val parseFunc = """(^\s*[a-zA-Z]+[0-9]*)""".r.unanchored
  //
  // /** string => (Ops, int)
  //   *
  //   * Returns if possible (Ops, length of string prefix to remove).
  //   *
  //   * {{{
  //   *   Arg.tokenize.apply("    + 2") // (Ops.Add, 6)
  //   * }}}
  //   */
  // def tokenize: PartialFunction[String, (OpApplicable, Int)] =
  //   case parseAdd(m)                        => (Ops.Add, m.length)
  //   case parseSub(m)                        => (Ops.Sub, m.length)
  //   case parseMul(m)                        => (Ops.Mul, m.length)
  //   case parseDiv(m)                        => (Ops.Div, m.length)
  //   case parseExponent(m)                   => (Ops.Exp, m.length)
  //   case parseFunc(m) if supported(m.strip) => (Ops.Func(m.strip), m.length)
  //
  // def getOpAction: PartialFunction[ OpApplicable, List[Arg] => List[Arg]] =
  //   case Ops.Add     => Ops.Add.apply
  //   case Ops.Sub     => Ops.Sub.apply
  //   case Ops.Mul     => Ops.Mul.apply
  //   case Ops.Div     => Ops.Div.apply
  //   case Ops.Exp     => Ops.Exp.apply
  //   case x: Ops.Func => x.apply
  //
  // def opHasPrecedence(op: Ops, prior: Option[Ops]) =
  //   prior match
  //     case None                           => false
  //     // case Some(_: LeftBracketPrecedence) => false
  //     case Some(o)                        => o.precedence > op.precedence
