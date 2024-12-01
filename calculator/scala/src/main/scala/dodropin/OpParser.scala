package dodropin

object OpParser:
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
    case parseAdd(m)                            => (Ops.Add, m.length)
    case parseSub(m)                            => (Ops.Sub, m.length)
    case parseMul(m)                            => (Ops.Mul, m.length)
    case parseDiv(m)                            => (Ops.Div, m.length)
    case parseExponent(m)                       => (Ops.Exp, m.length)

  def getOpAction: PartialFunction[OpApplicable, List[Arg] => List[Arg]] =
    case Ops.Add     => Ops.Add.apply
    case Ops.Sub     => Ops.Sub.apply
    case Ops.Mul     => Ops.Mul.apply
    case Ops.Div     => Ops.Div.apply
    case Ops.Exp     => Ops.Exp.apply

  def opHasPrecedence(op: Precedence, prior: Option[Precedence]) =
    prior match
      case None => false
      case Some(_: Bracket.Left) => false
      case Some(o) => o.precedence > op.precedence
