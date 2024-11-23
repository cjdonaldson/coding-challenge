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
  private val parseDouble = """([0-9]+\.[0-9]+)""".r
  private val parseInt = """([0-9]+)""".r

  def parse: PartialFunction[String, Arg] =
    case parseDouble(x) => Arg.ADbl(x.toDouble)
    case parseInt(x)    => Arg.AInt(x.toInt)
