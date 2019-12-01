package csvparser

import scala.io.Source
import TokenParser._

/**
 * Incremental parser that can generate an iterator over an input source. The iterator will parse the csv line by line,
 * keeping the memory usage constant.
 * The parser does not validate any line so it is for instance possible that one line in the csv contained 5 fields and the next one 6:
 * all lines are simply returned as they were parsed.
 * Higher order functions such as foreach and foldLeft are available on the returned iterator.
 */
object CsvParser  {

  abstract class CsvIterator(headers: Option[Seq[String]]) extends Iterator[Seq[String]] {
    def getHeaders: Seq[String] = headers match {
      case Some(headers) => headers
      case None          => sys.error("No headers defined for this csv.")
    }
  }

  /**
   * Returns an iterator over parsed csv lines that takes defaults for quote: '"', delimiter: ',' and line feed: the line feed character.
   * @param input         A stream pointing to the csv file
   * @param withHeaders   If true, the first line is expected to be a headers line (with same quotes, delimiter, line feed as a normal line)
   * @return              A CsvIterator over the parsed csv lines. If withHeaders was true, the CsvIterator's getHeaders method will return the headers.
   */
  def getIterator(input: Source, withHeaders: Boolean = false): CsvIterator = getIterator(input, "\"", ",", "\n", withHeaders)

  /**
   * @see getIterator(Source, Boolean) but with custom quote, delimiter and lineFeed.
   *
   * Note: the custom values for quote, delimiter and lineFeed are not validated, so it is possible to call this method with invalid combinations,
   * such as e.g. using the same token for both quote and delimiter.
   */
  def getIterator(input: Source, quote: String, delimiter: String, lineFeed: String, withHeaders: Boolean): CsvIterator = {

    // The implementation uses a state machine that knows 3 states: Inside Field (the default state), Inside Quotes and LineParsed.
    def transition(currentState: ParseState, c: Char): ParseState =
      currentState match {
        case InsideField(fields, currentField, quoteMatch, delimiterMatch, lineFeedMatch) =>
          // possible transitions, in order of prevalence: quote -> InsideQuotes, delimiter -> new Inside Field, -> line feed -> LineParsed
          parseNext(quote, quoteMatch, c) match {
            case Success(_) =>
              InsideQuotes(fields, dropMatchedChars(currentField, quoteMatch), "")
            case PartialResult(nextQuoteMatch) =>
              parseNext(delimiter, delimiterMatch, c) match {
                case Success(_) =>
                  val newField = dropMatchedChars(currentField, delimiterMatch)
                  InsideField (fields :+ newField, "")
                case PartialResult(nextDelimiterMatch) =>
                  parseNext (lineFeed, lineFeedMatch, c) match {
                    case Success(_) =>
                      val newField = dropMatchedChars(currentField, lineFeedMatch)
                      LineParsed(fields :+ newField)
                    case PartialResult(nextLineFeedMatch) =>
                      InsideField(fields, currentField :+ c, nextQuoteMatch, nextDelimiterMatch, nextLineFeedMatch)
                  }
              }
          }
        case InsideQuotes(fields, currentField, quoteMatch) =>
          // possible transitions: quote -> Inside Field.
          parseNext(quote, quoteMatch, c) match {
            case Success(_) =>
              InsideField(fields, dropMatchedChars(currentField, quoteMatch))
            case PartialResult(nextQuoteMatch) =>
              InsideQuotes(fields, currentField :+ c, nextQuoteMatch)
          }
        case s => sys.error(s"Illegal state: $s")
      }

    def parseLine(it: Iterator[Char]): Seq[String] = {
      val startingState: ParseState = InsideField(Vector(), "")

      val finalState = Iterator.iterate(startingState) {
        case s: LineParsed      => s
        case s if !it.hasNext   => LineParsed(s.getFields)
        case s                  => transition(s, it.next())
      }.dropWhile(!_.isInstanceOf[LineParsed])
        .next()

      finalState.getFields
    }

    val it = input.iterator

    val headers = if (withHeaders) {
      Some(parseLine(it))
    } else {
      None
    }

    new CsvIterator(headers) {
      override def hasNext: Boolean = it.hasNext

      override def next(): Seq[String] = parseLine(it)
    }
  }

  private sealed trait ParseState {
    def getFields: Vector[String]
  }

  private case class InsideField(fields: Vector[String], currentField: String, matchedQuote: String = "", matchedDelimiter: String = "", matchedLineFeed: String = "") extends ParseState {
    override def getFields: Vector[String] = fields :+ currentField
  }

  private case class InsideQuotes(fields: Vector[String], currentField: String, matchedQuote: String) extends ParseState {
    override def getFields: Vector[String] = fields :+ currentField
  }

  private case class LineParsed(fields: Vector[String]) extends ParseState  {
    override def getFields: Vector[String] = fields
  }
}