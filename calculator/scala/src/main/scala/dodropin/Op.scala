package dodropin

import scala.util.Try

/** Returns encoding of PEMDAS / GEMS and functions (named entities).
  */
sealed trait Op extends OpApplicable

object Op:
  object Add extends Op with Precedence.AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x + y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x + y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x + y) :: rest

    override val toString: String = "+"

  object Sub extends Op with Precedence.AddPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x - y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x - y) :: rest

    override val toString: String = "-"

  object Mul extends Op with Precedence.MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest => Arg.ADec(x * y) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest => Arg.ADec(x * y) :: rest

    override val toString: String = "*"

  object Div extends Op with Precedence.MulPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADec(y) :: Arg.ADec(x) :: rest => Arg.ADec(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADec(x) :: rest =>
        Arg.ADec(x / BigDecimal(y)) :: rest
      case Arg.ADec(y) :: Arg.AInt(x) :: rest =>
        Arg.ADec(BigDecimal(x) / y) :: rest

    override val toString: String = "/"

  object Exp extends Op with Precedence.ExpPrecedence:
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

  def supported(m: String): Boolean =
    Try(Op.Func(m).apply.isDefinedAt(List(Arg.ADec(1)))).toOption
      .contains(true)

  case class Func private (fn: String)
      extends Op
      with Precedence.FuncPrecedence:
    val apply: PartialFunction[List[Arg], List[Arg]] =
      fn match
        case "sqr" => {
          case Arg.AInt(x) :: rest =>
            Arg.AInt(x * x) :: rest
          case Arg.ADec(x) :: rest =>
            Arg.ADec(x * x) :: rest
        }
        case "sqrt" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADec(scala.math.sqrt(x.toDouble)) :: rest
          case Arg.ADec(x) :: rest =>
            Arg.ADec(scala.math.sqrt(x.toDouble)) :: rest
        }
        case "sin" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADec(scala.math.sin(x.toDouble)) :: rest
          case Arg.ADec(x) :: rest =>
            Arg.ADec(scala.math.sin(x.toDouble)) :: rest
        }
        case "cos" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADec(scala.math.cos(x.toDouble)) :: rest
          case Arg.ADec(x) :: rest =>
            Arg.ADec(scala.math.cos(x.toDouble)) :: rest
        }
        case "tan" => {
          case Arg.AInt(x) :: rest =>
            Arg.ADec(scala.math.tan(x.toDouble)) :: rest
          case Arg.ADec(x) :: rest =>
            Arg.ADec(scala.math.tan(x.toDouble)) :: rest
        }

    override def toString: String = fn

  object Func {
    def apply(s: String): Func = new Func(s.strip)
  }

  def getOpAction: PartialFunction[OpApplicable, List[Arg] => List[Arg]] =
    case Op.Add     => Op.Add.apply
    case Op.Sub     => Op.Sub.apply
    case Op.Mul     => Op.Mul.apply
    case Op.Div     => Op.Div.apply
    case Op.Exp     => Op.Exp.apply
    case x: Op.Func => x.apply
