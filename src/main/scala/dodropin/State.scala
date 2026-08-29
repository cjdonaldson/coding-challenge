package dodropin

case class State(output: List[Arg], ops: List[OpApplicable])

object State:
  def empty: State = State(List.empty, List.empty)
