package dodropin

// no mutable state; programmed to immutable and was bit
// import scala.collection.mutable.Stack
import scala.annotation.tailrec
import scala.util.Try
import javax.management.openmbean.OpenMBeanAttributeInfo
import scala.util.Success
import scala.util.Failure

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
    println(s"shunting -> $input $state\n")
    if (input.isEmpty) state
    else
      // val xx = BracketsParser.tokenize.apply(input)
      val (st, remove) =
        Arg.tokenize
          .orElse(BracketsParser.tokenize)
          .orElse(OpsParser.tokenize)
          .orElse { case err =>
            (Arg.Err(s"error: unhandled input: $err"), err.length)
          }
          .andThen {
            case (bracket: Brackets.LeftBracketPrecedence, remove) =>
              (State(state.output, bracket :: state.ops), remove)

            case (bracket: Brackets.RightBracketPrecedence, remove) =>
              val State(output, ops) = runOpTil(bracket.leftBracket, state)
              (State(output, ops), remove)

            case (arg: Arg, remove) =>
              (State(arg :: state.output, state.ops), remove)

            case (op: OpApplicable, remove) =>
              val nextState = runTopPrecedences(op, state)
              (State(nextState.output, op :: nextState.ops), remove)

            // case (x, remove) =>
            //   println(s"here $x, $remove")
            //   throw new Exception("ooop")
          }
          .apply(input)

      shuntingLoop(input.drop(remove), st)

  /** Unwinds the State producing a final result
    */
  @tailrec
  private def shuntingUnwrap(state: State): String =
    println(s"unwrap -> $state")
    def remainingVars =
      s"oops vars left: [${state.output.mkString(", ")}], no ops to reduce"

    def opsError = s"unhandled op: ${state.ops.mkString(", ")}"

    println(s"unwrap output ${state.output.length} ${state.output}")
    println(s"unwrap pos    ${state.ops.length} ${state.ops}")
    println

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
  private def runTopPrecedences(op: OpApplicable, state: State): State =
    println(s"runTopPrecedences has ${OpsParser
        .opHasPrecedence(op, state.ops.headOption)}: op: $op state: $state")
    println(
      s"runTopPrecedences ${op.precedence} ${state.ops.map(_.precedence)}"
    )
    if (OpsParser.opHasPrecedence(op, state.ops.headOption))
      runTopPrecedences(op, runOpOne(state))
    else state

  /** Returns State with top operation performed.
    */
  private def runOpOne(state: State): State =
    println(s"runOpOne state $state")
    if (state.ops.isEmpty) state
    else
      State(
        runOpOne(state.ops.head, state.output),
        state.ops.tail
      )

    //   case Nil        => state
    //   case op :: rest => State(
    //     runOpOne(op, state.output),
    //     rest
    //   )

  /** Returns modified List of Arg with op operation performed.
    */
  private def runOpOne(op: OpApplicable, output: List[Arg]): List[Arg] =
    def opErr =
      s"error: could not evaluate: $op using: '${output.mkString(", ")}'"

    def opUnhandled = s"error: unhandled operation: $op"

    println(s"runOpOne xx $op , ${output}")

    OpsParser.getOpAction
      .andThen { action =>
        // println(s"run action $action $op")
        Try(action(output))
          .getOrElse(List(Arg.Err(opErr)))
      }
      .orElse(_ => List(Arg.Err(opUnhandled)))
      .apply(op)

  // @tailrec
  private def runOpTil(op: OpApplicable, state: State): State =
    println(s"runOpTil [$op] ([${ state.output.mkString(", ") }] [${ state.ops.mkString(", ") }])")
    println(s"runOpTil top ${ state.ops.headOption }")
    println(s"runOpTil top ${ state.ops.headOption.map(_.isInstanceOf[OpApplicable]) }")
    println(s"runOpTil top ${ state.ops.headOption.map(_.isInstanceOf[Ops]) }")
    println(s"runOpTil ops ${ state.ops.isInstanceOf[List[OpApplicable]] }")

    if (state.ops.isEmpty)
      state

    else if (state.ops.head == op)
      state.copy(ops = state.ops.tail)

    else
      runOpTil(op, runOpOne(state))

    // def x: State = (state.ops: List[OpApplicable]) match
    //   case Nil =>
    //     println("runOpTil exit nil")
    //     state
    //
    //   // case ( sop: Arg ) :: rest =>
    //   //   throw new Exception("arg fail")
    //   //   // println(s"runOpTil exit match ${ state.copy(ops = rest) }")
    //   //   // state.copy(ops = rest)
    //
    //   case sop :: rest if sop == op =>
    //     println(s"runOpTil exit match ${ state.copy(ops = rest) }")
    //     state.copy(ops = rest)
    //
    //   case _ =>
    //     println(s"runOpTil another go $op $state")
    //     runOpTil(op, runOpOne(state))
    //
    // Try(x) match {
    //   case Success(x) =>
    //     println(s"success $x")
    //     x
    //   case Failure(x) =>
    //     println(s"fail $x")
    //     throw new Exception("ssddssd")
    // }

}
