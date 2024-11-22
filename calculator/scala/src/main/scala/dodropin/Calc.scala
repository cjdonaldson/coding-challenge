package dodropin

import scala.collection.mutable.Stack
import scala.util.Try

object Calc extends App {}

object CalcOps {
  def apply(s: String): String = {
    val parseDouble = """([0-9]+\.[0-9]+)""".r
    val parseInt = """([0-9]+)""".r

    val shunted = s
      .split(" ")
      .foldLeft(Stacks.empty) { case (Stacks(output, ops), token) =>
        token match {
          case "+"            => Stacks(output, ops.push(Ops.Add))
          case "-"            => Stacks(output, ops.push(Ops.Sub))
          case "*"            => Stacks(output, ops.push(Ops.Mul))
          case "/"            => Stacks(output, ops.push(Ops.Div))
          case parseDouble(x) => Stacks(output.push(Arg.ADbl(x.toDouble)), ops)
          case parseInt(x)    => Stacks(output.push(Arg.AInt(x.toInt)), ops)
          case x              => throw new Exception(s"no parse for $x")
        }
      }

    def getOutput(output: Arg): String =
      output match {
        case Arg.ADbl(d) => d.toString
        case Arg.AInt(i) => i.toString
      }

    def add(stack: Stack[Arg]): Stack[Arg] = {
      (stack.pop, stack.pop) match {
        case (Arg.AInt(x), Arg.AInt(y)) => stack.push(Arg.AInt(x + y))
        case (Arg.ADbl(x), Arg.ADbl(y)) => stack.push(Arg.ADbl(x + y))
        case (Arg.AInt(x), Arg.ADbl(y)) => stack.push(Arg.ADbl(x + y))
        case (Arg.ADbl(x), Arg.AInt(y)) => stack.push(Arg.ADbl(x + y))
      }
    }

    def loop(output: Stack[Arg], ops: Stack[Ops]): String =
      (output.length, ops.length) match {
        case (0, _) => "oops no value"
        case (1, 0) => getOutput(output.pop)
        case (_, _) => loop(add(output), ops.drop(1))
      }

    loop(shunted.output, shunted.ops)
  }
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
}

case class Stacks(output: Stack[Arg], ops: Stack[Ops])

object Stacks {
  def empty: Stacks = Stacks(Stack.empty, Stack.empty)
}
