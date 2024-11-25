package dodropin

// no mutable state; programmed to immutable and was bit
// import scala.collection.mutable.Stack
import scala.annotation.tailrec
import scala.util.Try

object CalcOps {
  def apply(fragments: List[String]): String =
    shuntingUnwrap(
      fragments.foldLeft(State.empty) { case (state, fragment) =>
        shuntingLoop(fragment, state)
      }
    )

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
            case (Ops.ParenRight, remove) =>
              val State(output, ops) = runOpTil(Ops.ParenLeft, state)
              (State(output, ops), remove)

            case (Ops.CurlyRight, remove) =>
              val State(output, ops) = runOpTil(Ops.CurlyLeft, state)
              (State(output, ops), remove)

            case (Ops.SquarRight, remove) =>
              val State(output, ops) = runOpTil(Ops.SquarLeft, state)
              (State(output, ops), remove)

            case (arg: Arg, remove) =>
              (State(arg :: state.output, state.ops), remove)

            case (op: Ops, remove) =>
              if (Ops.opHasPrecedence(op, state.ops.headOption))
                val State(output, ops) = runOpOne(state)
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
          case op :: rest => shuntingUnwrap(runOpOne(state))
          case _          => opsError

      case nil => "error: no equation to evaulate"

  /** Returns Stacks with top operation performed.
    */
  private def runOpOne(state: State): State =
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

  @tailrec
  private def runOpTil(op: Ops, state: State): State =
    state.ops match
      case sop :: rest if sop == op =>
        state.copy(ops = rest)
      case _ =>
        runOpTil(op, runOpOne(state))

}
