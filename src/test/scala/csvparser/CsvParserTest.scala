package csvparser

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class CsvParserTest extends FlatSpec with Matchers {
  import CsvParser._

  "parse" should "return an iterator of the parsed csv lines" in {
    val csvLine = "\"foo\",bar\n"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("foo", "bar")
  }

  "parse" should "handle end of input without line feed" in {
    val csvLine = "\"foo\",bar"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("foo", "bar")
  }

  "parse" should "handle multiple lines" in {
    val csvLine = "\"foo\",bar\n\"alice\",bob"

    val it = getIterator(Source.fromString(csvLine))

    it.toList shouldBe List(Seq("foo", "bar"), Seq("alice", "bob"))
  }

  "parse" should "handle a single field line" in {
    val csvLine = "bar\n"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("bar")
  }

  "parse" should "handle empty lines" in {
    val csvLine = "\n"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("")
  }

  "parse" should "handle empty fields" in {
    val csvLine = ",bar,,\"\",\n"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("", "bar", "", "", "")
  }

  "parse" should "parse a field containing quoted subsections" in {
    val csvLine = "\"foo\"bar\"foo\",bar\n"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("foobarfoo", "bar")
  }

  "parse" should "parse a quoted field containing separators and line feeds" in {
    val csvLine = "\"foo,bar\nfoo\",\"foo\"\"bar\"\n"

    val it = getIterator(Source.fromString(csvLine))

    it.hasNext shouldBe true
    it.next() shouldBe Seq("foo,bar\nfoo", "foobar")
  }

  "parse" should "parse multi character quotes, separators or line feeds" in {
    val quote = "quote"
    val separator = "sep"
    val linefeed = "lf"
    val csvLine = "\"foo,bar\nfoo\",bar\n"
      .replace("\"", quote)
      .replace(",", separator)
      .replace("\n", linefeed)

    val it = getIterator(Source.fromString(csvLine), quote, separator, linefeed, false)

    it.hasNext shouldBe true
    it.next() shouldBe Seq("foosepbarlffoo", "bar")
  }

  "parse" should "parse overlapping multi character quotes, separators or line feeds" in {
    val quote = "quote"
    val separator = "sep"
    val linefeed = "lf"
    val csvLine = "quoquotefooquotesesepbarllf"

    val it = getIterator(Source.fromString(csvLine), quote, separator, linefeed, false)

    it.hasNext shouldBe true
    it.next() shouldBe Seq("quofoose", "barl")
  }

  "parse" should "treat the first line as headers when withHeaders is true" in {
    val csvLine = "\"foo\",bar\n\"alice\",bob"

    val it = getIterator(Source.fromString(csvLine), true)

    it.getHeaders shouldBe Seq("foo", "bar")
    it.toList shouldBe List(Seq("alice", "bob"))
  }
}
