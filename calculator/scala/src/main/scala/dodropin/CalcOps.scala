package dodropin

// no mutable state; against the FP way, I programmed to mutable and was bit
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
    if (input.strip.isEmpty) state
    else
      val (st, remove) =
        Arg.tokenize
          .orElse(BracketParser.tokenize)
          .orElse(OpParser.tokenize)
          .orElse { case err =>
            (Arg.Err(s"error: unhandled input: [$err]"), err.length)
          }
          .andThen {
            case (left: Bracket.Left, remove) =>
              (State(state.output, left :: state.ops), remove)

            case (right: Bracket.Right, remove) =>
              val State(output, ops) = runOpTil(right.leftBracket, state)
              (State(output, ops), remove)

            case (arg: Arg, remove) =>
              (State(arg :: state.output, state.ops), remove)

            case (op: Op, remove) =>
              val nextState = runTopPrecedences(op, state)
              (State(nextState.output, op :: nextState.ops), remove)
          }
          .apply(input)

      shuntingLoop(input.drop(remove), st)

  /** Unwinds the State producing a final result
    */
  @tailrec
  private def shuntingUnwrap(state: State): String =
    def opsError = s"unhandled op: ${state.ops.mkString(", ")}"

    object argErr:
      def unapply(state: State): Option[String] =
        state.output match
          case Arg.Err(err) :: _ => Some(err)
          case _                 => None

    object noEquation:
      def unapply(state: State): Boolean =
        state == State.empty

    object result:
      def unapply(state: State): Option[Arg] =
        if (state.output.length == 1 && state.ops.isEmpty)
          state.output.headOption
        else None

    object unwrapMore:
      def unapply(state: State): Boolean =
        (state.output.nonEmpty && state.ops.nonEmpty)

    state match
      case argErr(err)    => err
      case noEquation()   => "error: no equation to evaulate"
      case result(output) => output.toString
      case unwrapMore()   => shuntingUnwrap(runOpOne(state))
      case _              => opsError

  /** Returns State with top operation performed as long as it has precedence.
    */
  private def runTopPrecedences(op: Op, state: State): State =
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

    def opUnhandled = s"error: unhandled operation: [$op]"

    Op.getOpAction
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
