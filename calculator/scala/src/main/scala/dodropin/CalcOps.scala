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
        Arg.tokenize
          .orElse(Ops.tokenize)
          .orElse { case err =>
            (Arg.Err(s"unhandled input: $err"), err.length)
          }
          .andThen {
            case (left: Bracket.Left, remove) =>
              (State(state.output, left :: state.ops), remove)

            case (right: Bracket.Right, remove) =>
              val State(output, ops) = runOpTil(right.leftBracket, state)
              (State(output, ops), remove)

            case (arg: Arg, remove) =>
              (State(arg :: state.output, state.ops), remove)

            case (op: Ops, remove) =>
              val nextState = runTopPrecedences(op, state)
              (State(nextState.output, op :: nextState.ops), remove)
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

      case Nil if state.ops.isEmpty => "error: no equation to evaulate"

      case output :: Nil if state.ops.isEmpty => output.toString

      case _ =>
        state.ops match
          case op :: rest => shuntingUnwrap(runOpOne(state))
          case _          => opsError

  /** Returns State with top operation performed as long as it has precedence.
    */
  private def runTopPrecedences(op: Ops, state: State): State =
    if (Precedence.opHasPrecedence(op, state.ops.headOption))
      runTopPrecedences(op, runOpOne(state))
    else state

  /** Returns State with top operation performed.
    */
  private def runOpOne(state: State): State =
    State(runOpOne(state.ops.head, state.output), state.ops.tail)

  /** Returns modified List of Arg with op operation performed.
    */
  private def runOpOne(op: OpApplicable, output: List[Arg]): List[Arg] =
    def opErr =
      s"error: could not evaluate: $op using: '${output.mkString(", ")}'"

    def opUnhandled = s"error: unhandled operation: $op"

    Ops.getOpAction
      .andThen { action =>
        Try(action(output))
          .getOrElse(List(Arg.Err(opErr)))
      }
      .orElse(_ => List(Arg.Err(opUnhandled)))
      .apply(op)

  @tailrec
  private def runOpTil(op: OpApplicable, state: State): State =
    state.ops match
      case sop :: rest if sop == op =>
        state.copy(ops = rest)
      case _ =>
        runOpTil(op, runOpOne(state))

}
