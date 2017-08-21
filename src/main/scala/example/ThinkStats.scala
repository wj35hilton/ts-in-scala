package example

import example.lexer.DictionaryLexer
import example.lexer.DictionaryToken

import example.parser.{DataDictionary, DictionaryParser}
import scala.util.parsing.combinator._

object TestDictParser {

  def testString = """
infile dictionary {
    _column(1)  str12    caseid     %12s  "RESPONDENT ID NUMBER"
    _column(13) byte     pregordr   %2f  "PREGNANCY ORDER (NUMBER)"
}
"""

  def doSomeStuffWithDict(dict: DataDictionary): Integer = {
    println(dict)

   11
  }

  def main(args: Array[String]) = {

    def res = for {
      ts <- DictionaryLexer(testString)
      dd <- DictionaryParser(ts)
      s = doSomeStuffWithDict(dd)
    } yield(s)
    println("RES: " + res)
  }
}
