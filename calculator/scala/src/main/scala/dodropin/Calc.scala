package dodropin

// no mutable state; programmed to immutable and was bit
// import scala.collection.mutable.Stack
import scala.annotation.tailrec
import scala.util.Try

object Calc extends App {}

object CalcOps {
  def apply(s: String): String =
    val shunted =
      @tailrec
      def loop(input: String, stacks: Stacks): Stacks =
        if (input.isEmpty) stacks
        else
          val (st, remove) =
            Ops.tokenize
              .orElse(Arg.tokenize)
              .orElse { case x =>
                (Arg.Err(x), x.length)
              }
              .andThen {
                case (a: Arg, remove) =>
                  (Stacks(a :: stacks.output, stacks.ops), remove)

                case (o: Ops, remove) =>
                  (Stacks(stacks.output, o :: stacks.ops), remove)
              }
              .apply(input)

          loop(input.drop(remove), st)

      loop(s, Stacks.empty)

    @tailrec
    def loop(stacks: Stacks): String =
      def remainingVars =
        s"oops vars left: [${stacks.output.mkString(", ")}], no ops to reduce"

      def opsError = s"unhandled op: ${stacks.ops.mkString(", ")}"

      def runFn(op: Ops, rest: List[Ops]): Stacks =
        Ops.getOpAction
          .andThen { fn =>
            val output = Try(fn(stacks.output))
              .getOrElse(
                List(Arg.Err(s"error: could not evaluate '${stacks.output
                    .mkString(" ")} $op'"))
              )
            Stacks(output, rest)
          }
          .orElse(_ =>
            Stacks(List(Arg.Err(s"error: unhandled operation: $op")), Nil)
          )
          .apply(op)

      stacks.output match
        case Arg.Err(err) :: _                   => err
        case output :: nil if stacks.ops.isEmpty => output.toString
        case _ :: _ =>
          stacks.ops match
            case op :: rest => loop(runFn(op, rest))
            case _          => opsError

        case nil => "error: no equation to evaulate"

    loop(shunted)
}
