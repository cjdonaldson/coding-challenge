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

  object Sub extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x - y) :: rest

  object Mul extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x * y) :: rest

  object Div extends Ops:
    def apply: PartialFunction[List[Arg], List[Arg]] =
      case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
      case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
      case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x / y) :: rest

  def parse: PartialFunction[String, Ops] =
    case "+" => Ops.Add
    case "-" => Ops.Sub
    case "*" => Ops.Mul
    case "/" => Ops.Div

  def getOpAction: PartialFunction[Ops, List[Arg] => List[Arg]] =
    case Ops.Add => Ops.Add.apply
    case Ops.Sub => Ops.Sub.apply
    case Ops.Mul => Ops.Mul.apply
    case Ops.Div => Ops.Div.apply
