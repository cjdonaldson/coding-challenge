package dodropin

// no mutable state; programmed to immutable and was bit
// import scala.collection.mutable.Stack
import scala.annotation.tailrec
import scala.util.Try

object Calc extends App {}

object CalcOps {
  def apply(s: String): String =
    val shunted: Stacks = s
      // TODO: this is naive; and Regex is painful; so need a better tokenizer
      .split(" ")
      .foldLeft(Stacks.empty) { case (Stacks(output, ops), token) =>
        Ops.parse
          .orElse(Arg.parse)
          .orElse { case x =>
            throw new Exception(s"no parse for $x")
          }
          .andThen {
            case a: Arg => Stacks(a :: output, ops)
            case o: Ops => Stacks(output, o :: ops)
          }
          .apply(token)
      }

    @tailrec
    def loop(stacks: Stacks): String =
      def remainingVars =
        s"oops vars left: [${stacks.output.mkString(", ")}], no ops to reduce"

      def remainingOps = s"unhandled op: ${stacks.ops.mkString(", ")}"

      stacks.output match
        case x :: nil if stacks.ops.isEmpty => x.toString
        case _ :: _ =>
          stacks.ops match
            case op :: rest =>
              val fn = Ops.getOpAction
                .andThen(fn => Stacks(fn(stacks.output), rest))
                .orElse(_ => Stacks(List(Arg.Err(s"unhandled op: $op")), Nil))
                .apply(op)

              loop(fn)
            case _ => remainingOps

        case nil => "oops no value"

    loop(shunted)
}
