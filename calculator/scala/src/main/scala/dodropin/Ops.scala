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
sealed trait Ops:
  def apply: PartialFunction[List[Arg], List[Arg]]

  def precedence: Int

object Ops:
  private val parenPrecedence = 5

  object Add extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x + y) :: rest

    override val precedence: Int = 1

    override def toString: String = "+"

  object Sub extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x - y) :: rest

    override val precedence: Int = 1

    override def toString: String = "-"

  object Mul extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x * y) :: rest

    override val precedence: Int = 2

    override def toString: String = "*"

  object Div extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x / y) :: rest

    override val precedence: Int = 2

    override def toString: String = "/"

  object Exp extends Ops:
    import scala.math.pow
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(pow(x, y).toInt) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(pow(x, y)) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(pow(x, y)) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(pow(x, y)) :: rest

    override val precedence: Int = 4

    override def toString: String = "^"

  object ParenLeft extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override val precedence: Int = parenPrecedence

    override def toString: String = "("

  object ParenRight extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override val precedence: Int = parenPrecedence

    override def toString: String = ")"

  object CurlyLeft extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override val precedence: Int = parenPrecedence

    override def toString: String = "{"

  object CurlyRight extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override val precedence: Int = parenPrecedence

    override def toString: String = "]"

  object SquarLeft extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override val precedence: Int = parenPrecedence

    override def toString: String = "["

  object SquarRight extends Ops:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override val precedence: Int = parenPrecedence

    override val toString: String = "]"

  private val parseAdd = """(^\s*\+)""".r.unanchored
  private val parseSub = """(^\s*-)""".r.unanchored
  private val parseMul = """(^\s*\*)""".r.unanchored
  private val parseDiv = """(^\s*/)""".r.unanchored

  private val parseExponent = """(^\s*\^)""".r.unanchored

  private val parseParenLeft = """(^\s*\()""".r.unanchored
  private val parseCurlyLeft = """(^\s*\{)""".r.unanchored
  private val parseSquarLeft = """(^\s*\[)""".r.unanchored
  private val parseParenRight = """(^\s*\))""".r.unanchored
  private val parseCurlyRight = """(^\s*\})""".r.unanchored
  private val parseSquarRight = """(^\s*\])""".r.unanchored

  /** string => (Ops, int)
    *
    * Returns if possible (Ops, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("    + 2") // (Ops.Add, 6)
    * }}}
    */
  def tokenize: PartialFunction[String, (Ops, Int)] =
    case parseAdd(m)        => (Ops.Add, m.length)
    case parseSub(m)        => (Ops.Sub, m.length)
    case parseMul(m)        => (Ops.Mul, m.length)
    case parseDiv(m)        => (Ops.Div, m.length)
    case parseExponent(m)   => (Ops.Exp, m.length)
    case parseParenLeft(m)  => (Ops.ParenLeft, m.length)
    case parseCurlyLeft(m)  => (Ops.CurlyLeft, m.length)
    case parseSquarLeft(m)  => (Ops.SquarLeft, m.length)
    case parseParenRight(m) => (Ops.ParenRight, m.length)
    case parseCurlyRight(m) => (Ops.CurlyRight, m.length)
    case parseSquarRight(m) => (Ops.SquarRight, m.length)

  def getOpAction: PartialFunction[Ops, List[Arg] => List[Arg]] =
    case Ops.Add => Ops.Add.apply
    case Ops.Sub => Ops.Sub.apply
    case Ops.Mul => Ops.Mul.apply
    case Ops.Div => Ops.Div.apply
    case Ops.Exp => Ops.Exp.apply

  private val notThese = Set(
    ParenLeft,
    CurlyLeft,
    SquarLeft
  )

  def opHasPrecedence(op: Ops, prior: Option[Ops]) =
    prior
      .filterNot(p => notThese.exists(_ == p))
      .fold(false)(_.precedence > op.precedence)
