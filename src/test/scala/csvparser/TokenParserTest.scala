package csvparser

import org.scalatest.{FlatSpec, Matchers}

class TokenParserTest extends FlatSpec with Matchers {
  import TokenParser._

  "parseNext" should "return Success if the full token was matched" in {
    parseNext("token", "toke", 'n') shouldBe Success("token")
  }

  "parseNext" should "return a PartialResult if part of the token was matched" in {
    parseNext("token", "", 't') shouldBe PartialResult("t")
  }

  "parseNext" should "return the longest possible PartialResult if the new character makes the previous partial result invalid" in {
    parseNext("total", "tot", 'o') shouldBe PartialResult("to")

    parseNext("total", "tota", 'o') shouldBe PartialResult("")
  }
}
