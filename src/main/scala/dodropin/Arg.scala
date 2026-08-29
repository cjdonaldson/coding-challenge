package dodropin

enum Arg:
  case AInt(n: Int) extends Arg
  case ADec(d: BigDecimal) extends Arg
  case Err(err: String) extends Arg

  override def toString: String =
    this match
      case Arg.ADec(d) => d.toString
      case Arg.AInt(i) => i.toString
      case Arg.Err(s)  => s

object Arg:
  private val consts: Map[String, Arg] = Map(
    "pi" -> Arg.ADec(355.0 / 113.0),
    "tau" -> Arg.ADec(2.0 * 355.0 / 113.0),
    "e" -> Arg.ADec(2.718_281_828_459_045_235_36)
  )

  private val parseDecimal = """(^\s*[0-9]+\.[0-9]+\s*)""".r.unanchored
  private val parseInt = """(^\s*[0-9]+\s*)""".r.unanchored

  private object ParseConst:
    def unapply(s: String): Option[String] =
      """(^\s*[a-zA-Z]+[0-9]*\s*)""".r.unanchored
        .findFirstIn(s)
        .find(c => consts.isDefinedAt(c.strip))

  /** string => (Arg, int)
    *
    * Returns if possible (Arg, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("   5    + 2") // (Arg.AInt(5), 8)
    * }}}
    */
  def tokenize: PartialFunction[String, (Arg, Int)] =
    case parseDecimal(x) => (Arg.ADec(BigDecimal(x.strip)), x.length)
    case parseInt(x)     => (Arg.AInt(x.strip.toInt), x.length)
    case ParseConst(c)   => (consts(c.strip), c.length)
