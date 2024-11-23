package dodropin

// no mutable state; programmed to immutable and was bit
// import scala.collection.mutable.Stack
import scala.annotation.tailrec
import scala.util.Try

object Calc extends App {}

object CalcOps {
  def apply(s: String): String =
    val parseDouble = """([0-9]+\.[0-9]+)""".r
    val parseInt = """([0-9]+)""".r

    val shunted: Stacks = s
      .split(" ")
      .foldLeft(Stacks.empty) { case (Stacks(output, ops), token) =>
        token match
          case "+"            => Stacks(output, Ops.Add :: ops)
          case "-"            => Stacks(output, Ops.Sub :: ops)
          case "*"            => Stacks(output, Ops.Mul :: ops)
          case "/"            => Stacks(output, Ops.Div :: ops)
          case parseDouble(x) => Stacks(Arg.ADbl(x.toDouble) :: output, ops)
          case parseInt(x)    => Stacks(Arg.AInt(x.toInt) :: output, ops)
          case x              => throw new Exception(s"no parse for $x")
      }

    def getOutput(output: Arg): String =
      output match
        case Arg.ADbl(d) => d.toString
        case Arg.AInt(i) => i.toString
        case Arg.Err(s)  => s

    def add(stack: List[Arg]): List[Arg] =
      stack match
        case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x + y) :: rest
        case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
        case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x + y) :: rest
        case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x + y) :: rest
        case rest =>
          throw new Exception(s"unhandled ops ${rest.mkString(", ")}")

    def sub(stack: List[Arg]): List[Arg] =
      stack match
        case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x - y) :: rest
        case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
        case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x - y) :: rest
        case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x - y) :: rest
        case rest =>
          throw new Exception(s"unhandled ops ${rest.mkString(", ")}")

    def mul(stack: List[Arg]): List[Arg] =
      stack match
        case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x * y) :: rest
        case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
        case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x * y) :: rest
        case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x * y) :: rest
        case rest =>
          throw new Exception(s"unhandled ops ${rest.mkString(", ")}")

    def div(stack: List[Arg]): List[Arg] =
      stack match
        case Arg.AInt(y) :: Arg.AInt(x) :: rest => Arg.AInt(x / y) :: rest
        case Arg.ADbl(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
        case Arg.AInt(y) :: Arg.ADbl(x) :: rest => Arg.ADbl(x / y) :: rest
        case Arg.ADbl(y) :: Arg.AInt(x) :: rest => Arg.ADbl(x / y) :: rest
        case rest =>
          throw new Exception(s"unhandled ops ${rest.mkString(", ")}")

    @tailrec
    def loop(stacks: Stacks): String =
      def remainingVars =
        s"oops vars left: [${stacks.output.mkString(", ")}], no ops to reduce"

      def remainingOps = s"unhandled op: ${stacks.ops.mkString(", ")}"

      def process: Stacks =
        stacks.ops match
          case Ops.Add :: rest => Stacks(add(stacks.output), rest)
          case Ops.Sub :: rest => Stacks(sub(stacks.output), rest)
          case Ops.Mul :: rest => Stacks(mul(stacks.output), rest)
          case Ops.Div :: rest => Stacks(div(stacks.output), rest)
          case _               => Stacks(List(Arg.Err(remainingOps)), Nil)

      stacks.output match
        case x :: nil if stacks.ops.isEmpty => getOutput(x)
        case _ :: _ =>
          stacks.ops match
            case Ops.Add :: rest => loop(Stacks(add(stacks.output), rest))
            case Ops.Sub :: rest => loop(Stacks(sub(stacks.output), rest))
            case Ops.Mul :: rest => loop(Stacks(mul(stacks.output), rest))
            case Ops.Div :: rest => loop(Stacks(div(stacks.output), rest))
            case _               => remainingOps
        case nil => "oops no value"

    loop(shunted)
}

enum Ops {
  case Add extends Ops
  case Sub extends Ops
  case Mul extends Ops
  case Div extends Ops
}

enum Arg {
  case AInt(n: Int) extends Arg
  case ADbl(d: Double) extends Arg
  case Err(err: String) extends Arg
}

case class Stacks(output: List[Arg], ops: List[Ops])

object Stacks {
  def empty: Stacks = Stacks(List.empty, List.empty)
}
