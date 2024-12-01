package dodropin

import scala.util.Try

/** Returns encoding of PEMDAS / GEMS and functions (named entities).
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

  def supported(m: String): Boolean =
    Try(Ops.Func(m).apply.isDefinedAt(List(Arg.ADec(1)))).toOption
      .contains(true)

  case class Func private (fn: String)
      extends Ops
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
    case Ops.Add     => Ops.Add.apply
    case Ops.Sub     => Ops.Sub.apply
    case Ops.Mul     => Ops.Mul.apply
    case Ops.Div     => Ops.Div.apply
    case Ops.Exp     => Ops.Exp.apply
    case x: Ops.Func => x.apply
