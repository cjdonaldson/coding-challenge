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
sealed trait Ops extends OpApplicable

object Ops:
  object Add extends Ops with Precedence.AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x + y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x + y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x + y) :: rest

    override val toString: String = "+"

  object Sub extends Ops with Precedence.AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x - y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x - y) :: rest

    override val toString: String = "-"

  object Mul extends Ops with Precedence.MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x * y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x * y) :: rest

    override val toString: String = "*"

  object Div extends Ops with Precedence.MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest =>
        Arg.ADec(x / BigDecimal(y)) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest =>
        Arg.ADec(BigDecimal(x) / y) :: rest

    override val toString: String = "/"

  object Exp extends Ops with Precedence.ExpPrecedence:
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

    override val toString: String = "^"

  private val parseAdd = """(^\s*\+)""".r.unanchored
  private val parseSub = """(^\s*-)""".r.unanchored
  private val parseMul = """(^\s*\*)""".r.unanchored
  private val parseDiv = """(^\s*/)""".r.unanchored

  private val parseExponent = """(^\s*\^)""".r.unanchored

  /** string => (Ops, int)
    *
    * Returns if possible (Ops, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("    + 2") // (Ops.Add, 6)
    * }}}
    */
  def tokenize: PartialFunction[String, (OpApplicable, Int)] =
    case parseAdd(m)        => (Ops.Add, m.length)
    case parseSub(m)        => (Ops.Sub, m.length)
    case parseMul(m)        => (Ops.Mul, m.length)
    case parseDiv(m)        => (Ops.Div, m.length)
    case parseExponent(m)   => (Ops.Exp, m.length)

  def getOpAction: PartialFunction[OpApplicable, List[Arg] => List[Arg]] =
    case Ops.Add => Ops.Add.apply
    case Ops.Sub => Ops.Sub.apply
    case Ops.Mul => Ops.Mul.apply
    case Ops.Div => Ops.Div.apply
    case Ops.Exp => Ops.Exp.apply
