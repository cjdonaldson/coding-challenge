package dodropin

trait OpApplicable extends Precedence:
  def apply: PartialFunction[List[Arg], List[Arg]]
