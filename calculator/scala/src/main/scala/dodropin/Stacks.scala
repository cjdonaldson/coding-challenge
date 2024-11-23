package dodropin

case class Stacks(output: List[Arg], ops: List[Ops])

object Stacks:
  def empty: Stacks = Stacks(List.empty, List.empty)
