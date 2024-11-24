package dodropin

// no mutable state; programmed to immutable and was bit
// import scala.collection.mutable.Stack
import scala.annotation.tailrec
import scala.util.Try

object Calc extends App {}

object CalcOps {
  def apply(s: String): String =
    shuntingUnwrap(shuntingLoop(s, State.empty))

  @tailrec
  private def shuntingLoop(input: String, state: State): State =
    if (input.isEmpty) state
    else
      val (st, remove) =
        Ops.tokenize
          .orElse(Arg.tokenize)
          .orElse { case err =>
            (Arg.Err(s"unhandled input: $err"), err.length)
          }
          .andThen {
            case (arg: Arg, remove) =>
              (State(arg :: state.output, state.ops), remove)

            case (op: Ops, remove) =>
              if (Ops.opHasPrecedence(op, state.ops.headOption))
                val State(output, ops) = runTopOp(state)
                (State(output, op :: ops), remove)
              else (State(state.output, op :: state.ops), remove)
          }
          .apply(input)

      shuntingLoop(input.drop(remove), st)

  /** Unwinds the State producing a final result
    */
  @tailrec
  private def shuntingUnwrap(state: State): String =
    def remainingVars =
      s"oops vars left: [${state.output.mkString(", ")}], no ops to reduce"

    def opsError = s"unhandled op: ${state.ops.mkString(", ")}"

    state.output match
      case Arg.Err(err) :: _ => err

      case output :: nil if state.ops.isEmpty => output.toString

      case _ :: _ =>
        state.ops match
          case op :: rest => shuntingUnwrap(runTopOp(state))
          case _          => opsError

      case nil => "error: no equation to evaulate"

  /** Returns Stacks with top operation performed.
    */
  private def runTopOp(state: State): State =
    val (op, rest) = (state.ops.head, state.ops.tail)

    def opErr =
      s"error: could not evaluate: $op using: '${state.output.mkString(", ")}'"

    def opUnhandled = s"error: unhandled operation: $op"

    Ops.getOpAction
      .andThen { action =>
        val output = Try(action(state.output))
          .getOrElse(List(Arg.Err(opErr)))

        State(output, rest)
      }
      .orElse(_ => State(List(Arg.Err(opUnhandled)), Nil))
      .apply(op)

}
