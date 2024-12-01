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
  trait AddPrecedence extends Ops {
    def precedence: Int = 1
  }

  trait MulPrecedence extends Ops {
    def precedence: Int = 3
  }

  trait ExpPrecedence extends Ops {
    def precedence: Int = 4
  }

  trait ParenPrecedence extends Ops {
    def precedence: Int = 6
  }

  object Add extends AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x + y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x + y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x + y) :: rest

    override def toString: String = "+"

  object Sub extends AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x - y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x - y) :: rest

    override def toString: String = "-"

  object Mul extends MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x * y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x * y) :: rest

    override def toString: String = "*"

  object Div extends MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest =>
        Arg.ADec(x / BigDecimal(y)) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest =>
        Arg.ADec(BigDecimal(x) / y) :: rest

    override def toString: String = "/"

  object Exp extends ExpPrecedence:
    import scala.math.pow
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest =>
        Arg.AInt(pow(x, y).toInt) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest =>
        Arg.ADec(pow(x.toDouble, y.toDouble)) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest =>
        Arg.ADec(pow(x.toDouble, y.toDouble)) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest =>
        Arg.ADec(pow(x.toDouble, y.toDouble)) :: rest

    override def toString: String = "^"

  object ParenLeft extends ParenPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override def toString: String = "("

  object ParenRight extends ParenPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override def toString: String = ")"

  object CurlyLeft extends ParenPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override def toString: String = "{"

  object CurlyRight extends ParenPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override def toString: String = "]"

  object SquarLeft extends ParenPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

    override def toString: String = "["

  object SquarRight extends ParenPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case x => x

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
