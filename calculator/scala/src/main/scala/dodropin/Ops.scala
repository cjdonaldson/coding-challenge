package dodropin

sealed trait Ops: //  extends Product with Serializable:
  def apply: PartialFunction[List[Arg], List[Arg]]

object Ops:
  object Add extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x + y) :: rest

    override def toString: String = "+"

  object Sub extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x - y) :: rest

    override def toString: String = "-"

  object Mul extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x * y) :: rest

    override def toString: String = "*"

  object Div extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x / y) :: rest

    override def toString: String = "/"

  private val parseAdd = """(^\s*\+\s*)""".r.unanchored
  private val parseSub = """(^\s*-\s*)""".r.unanchored
  private val parseMul = """(^\s*\*\s*)""".r.unanchored
  private val parseDiv = """(^\s*/\s*)""".r.unanchored

  /** string => (Ops, int)
    *
    * Returns if possible (Ops, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("    + 2") // (Ops.Add, 6)
    * }}}
    */
  def tokenize: PartialFunction[String, (Ops, Int)] =
    case parseAdd(m) => (Ops.Add, m.length)
    case parseSub(m) => (Ops.Sub, m.length)
    case parseMul(m) => (Ops.Mul, m.length)
    case parseDiv(m) => (Ops.Div, m.length)

  def getOpAction: PartialFunction[Ops, List[Arg] => List[Arg]] =
    case Ops.Add => Ops.Add.apply
    case Ops.Sub => Ops.Sub.apply
    case Ops.Mul => Ops.Mul.apply
    case Ops.Div => Ops.Div.apply
