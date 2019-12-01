package csvparser

/**
 * Incremental parser that tries to match a single token
 */
object TokenParser {
  sealed trait TokenParseResult {
    def matched: String
  }

  case class Success(matched: String) extends TokenParseResult

  case class PartialResult(matched: String) extends TokenParseResult

  /**
   * Matches the next character together with the partialResult against the token.
   * @param partialMatch The characters that have been matched so far.
   * @param c The next character to match.
   * @return Success if the token is now fully matched, or a PartialResult that contains the part of the token that is matched so far.
   */
  def parseNext(token: String, partialMatch: String, c: Char): TokenParseResult = {
    val newMatched = partialMatch :+ c
    if (token == newMatched) {
      Success(token)
    } else {
      if (token.startsWith(newMatched)) {
        PartialResult(newMatched)
      } else {
        // try if any remainder of the earlier parsed characters contains the start of the token
        val bestMatch = newMatched.tail.foldLeft("") { case (partial, c) => parseNext(token, partial, c).matched }
        PartialResult(bestMatch)
      }
    }
  }

  /**
   * Removes the characters from the tail of String s that were matched int the parsed token.
   */
  def dropMatchedChars(s: String, parsed: String): String = s.take(s.length - parsed.length)
}
