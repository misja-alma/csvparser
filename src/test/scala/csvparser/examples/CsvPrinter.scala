package csvparser.examples

import java.io.File

import csvparser.CsvParser._

import scala.io.Source

/**
 * Example of the usage of CsvParser: this prints for each line in the csv the fields together with their header names.
 */
object CsvPrinter {
  def main(args: Array[String]): Unit = {
    require(args.length == 1, "Usage: CsvPrinter <filename>")

    val file = args.head
    val iterator = getIterator(Source.fromFile(new File(file), "UTF8"), true)
    val headers = iterator.getHeaders
    iterator foreach printCsvLine(headers)
  }

  def printCsvLine(headers: Seq[String])(line: Seq[String]): Unit = {
    headers.zip(line).foreach { case (header, field) => println (s"$header: $field")}
    println
  }
}
