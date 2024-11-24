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
                throw new Exception(s"no parse for $x")
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

      def remainingOps = s"unhandled op: ${stacks.ops.mkString(", ")}"

      stacks.output match
        case Arg.Err(err) :: _              => err
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
