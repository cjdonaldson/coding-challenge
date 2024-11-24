package dodropin

case class State(output: List[Arg], ops: List[Ops])

object State:
  def empty: State = State(List.empty, List.empty)
