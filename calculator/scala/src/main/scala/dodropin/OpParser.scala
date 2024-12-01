package dodropin

object OpParser:
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
    case parseAdd(m)                      => (Op.Add, m.length)
    case parseSub(m)                      => (Op.Sub, m.length)
    case parseMul(m)                      => (Op.Mul, m.length)
    case parseDiv(m)                      => (Op.Div, m.length)
    case parseExponent(m)                 => (Op.Exp, m.length)
    case parseFunc(m) if Op.supported(m) => (Op.Func(m), m.length)

  def getOpAction: PartialFunction[OpApplicable, List[Arg] => List[Arg]] =
    case Op.Add     => Op.Add.apply
    case Op.Sub     => Op.Sub.apply
    case Op.Mul     => Op.Mul.apply
    case Op.Div     => Op.Div.apply
    case Op.Exp     => Op.Exp.apply
    case x: Op.Func => x.apply

  def opHasPrecedence(op: Precedence, prior: Option[Precedence]) =
    prior match
      case None                  => false
      case Some(_: Bracket.Left) => false
      case Some(o)               => o.precedence > op.precedence
