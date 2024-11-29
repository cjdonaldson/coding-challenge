package dodropin

enum Arg:
  case AInt(n: Int) extends Arg
  case ADbl(d: Double) extends Arg
  case Err(err: String) extends Arg

  override def toString: String =
    this match
      case Arg.ADbl(d) => d.toString
      case Arg.AInt(i) => i.toString
      case Arg.Err(s)  => s

object Arg:
  private val parseDouble = """(^\s*[0-9]+\.[0-9]+\s*)""".r.unanchored
  private val parseInt = """(^\s*[0-9]+\s*)""".r.unanchored
  private val parseConst = """(^\s*[a-zA-Z]+[0-9]*\s*)""".r.unanchored

  /** string => (Arg, int)
    *
    * Returns if possible (Arg, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("   5    + 2") // (Arg.AInt(5), 8)
    * }}}
    */
  def tokenize: PartialFunction[String, (Arg, Int)] =
    case parseDouble(x) => (Arg.ADbl(x.strip.toDouble), x.length)
    case parseInt(x)    => (Arg.AInt(x.strip.toInt), x.length)
    case parseConst(c) =>
      c.strip match
        case "pi"  => (Arg.ADbl(22.0 / 7.0), c.length)
        case "tau" => (Arg.ADbl(2.0 * 22.0 / 7.0), c.length)
        case "e"   => (Arg.ADbl(2.718_281_828_459_045_235_36), c.length)
