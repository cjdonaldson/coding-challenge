package dodropin

object BracketParser:
  private val parseParenLeft = """(^\s*\()""".r.unanchored
  private val parseCurlyLeft = """(^\s*\{)""".r.unanchored
  private val parseSquarLeft = """(^\s*\[)""".r.unanchored
  private val parseParenRight = """(^\s*\))""".r.unanchored
  private val parseCurlyRight = """(^\s*\})""".r.unanchored
  private val parseSquarRight = """(^\s*\])""".r.unanchored

  /** string => (Brackets, int)
    *
    * Returns if possible (Brackets, length of string prefix to remove).
    *
    * {{{
    *   Arg.tokenize.apply("    + 2") // (Brackets.Add, 6)
    * }}}
    */
  def tokenize: PartialFunction[String, (Bracket, Int)] =
    case parseParenLeft(m)  => (Bracket.ParenLeft, m.length)
    case parseCurlyLeft(m)  => (Bracket.CurlyLeft, m.length)
    case parseSquarLeft(m)  => (Bracket.SquarLeft, m.length)
    case parseParenRight(m) => (Bracket.ParenRight, m.length)
    case parseCurlyRight(m) => (Bracket.CurlyRight, m.length)
    case parseSquarRight(m) => (Bracket.SquarRight, m.length)
