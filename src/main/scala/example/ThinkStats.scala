package example

import example.lexer.{DictionaryLexer, DictionaryToken}
import example.parser.{ColumnDefinition, DataDictionary, DictionaryParser}
import java.io.{FileInputStream}
import java.util.zip.{GZIPInputStream}
import scala.io.Source
import scala.util.parsing.combinator._

object TestDictParser {

  def testString = """
infile dictionary {
    _column(1)  str12    caseid     %12s  "RESPONDENT ID NUMBER"
    _column(13) byte     pregordr   %2f  "PREGNANCY ORDER (NUMBER)"
}
"""

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def extractColumn(huh: String, columnDef: ColumnDefinition): String = {
    val start = columnDef.column
    val end = columnDef.formatLen
    val name = columnDef.columnName
    s"$name: ${huh.drop(start).take(end)}"
  }

  def parseLine(huh: String, dict: DataDictionary): List[String] =
    dict.columnDefs.map(cd => extractColumn(huh, cd))

  def doSomeStuffWithDict(fileName: String, dict: DataDictionary): Seq[Seq[String]] = {
    using(Source.fromInputStream(new GZIPInputStream(new FileInputStream(fileName)))) {
      source => {
        val ls = source.getLines.toList
        for {
          l <- ls
        } yield {
          parseLine(l, dict)
        }
      }
    }
  }

  def main(args: Array[String]) = {
    using(Source.fromFile("/Users/whilton/work/praxis/ts-in-scala/data/2002FemPreg.dct")) {
      schemaSource => {
        val schema = schemaSource.getLines.mkString

        def res2 = for {
          ts <- DictionaryLexer(schema)
          dd <- DictionaryParser(ts)
          s = doSomeStuffWithDict("/Users/whilton/work/praxis/ts-in-scala/data/2002FemPreg.dat.gz", dd)
        } yield(s)

        res2 match {
          case Left(msg) => println("MSG: " + msg)
          case Right(recs) => println("RECS: " + recs.head)
        }
      }
    }
  }
}
