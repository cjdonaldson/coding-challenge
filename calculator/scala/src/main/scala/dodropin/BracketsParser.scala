package dodropin

import scala.util.Try

object BracketsParser:
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
  def tokenize: PartialFunction[String, (Brackets, Int)] =
    case parseParenLeft(m)  => (Brackets.ParenLeft, m.length)
    case parseCurlyLeft(m)  => (Brackets.CurlyLeft, m.length)
    case parseSquarLeft(m)  => (Brackets.SquarLeft, m.length)
    case parseParenRight(m) => (Brackets.ParenRight, m.length)
    case parseCurlyRight(m) => (Brackets.CurlyRight, m.length)
    case parseSquarRight(m) => (Brackets.SquarRight, m.length)
