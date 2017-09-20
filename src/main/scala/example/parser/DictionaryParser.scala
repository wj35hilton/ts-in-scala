package example.parser

import example.lexer._
import scala.util.parsing.combinator.{Parsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

case class DictionaryParserError(msg: String)

case class ColumnDefinition(
  column: Int,
  columnType: String,
  columnName: String,
  formatLen: Int,
  formatSpec: String,
  desc: String)

case class DataDictionary(columnDefs: List[ColumnDefinition]) {
  override def toString = "----> " + columnDefs
}

class DictionaryTokenReader(tokens: Seq[DictionaryToken]) extends Reader[DictionaryToken] {
  override def first: DictionaryToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[DictionaryToken] = new DictionaryTokenReader(tokens.tail)
}

object DictionaryParser extends Parsers {
  override type Elem = DictionaryToken

  private def format: Parser[FORMAT] = {
    accept("format", { case f @ FORMAT(_, _) => f })
  }

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifier", { case id @ IDENTIFIER(_) => id })
  }

  private def integer: Parser[INTEGER] = {
    accept("integer", { case i @ INTEGER(_) => i })
  }

  private def quoted: Parser[QUOTED] = {
    accept("quoted", { case q @ QUOTED(_) => q })
  }

  def column: Parser[Integer] = {
    OPEN_PAREN ~ integer ~ CLOSE_PAREN ^^ {case _ ~ size ~ _ => size.n}
  }

  def columnDef: Parser[ColumnDefinition] = {
    COLUMN_DES_KW ~ column ~ identifier ~ identifier ~ format ~ quoted ^^  { case _ ~ column ~ columnType ~ columnName ~ format ~ desc => ColumnDefinition(column - 1, columnType.str, columnName.str, format.length, format.spec, desc.str)}
  }

  def dictionary: Parser[DataDictionary] = {
    INFILE_KW ~ DICTIONARY_KW ~ OPEN_BRACKET ~ columnDef.* ~ CLOSE_BRACKET ^^ {case _ ~ _ ~ _ ~ columnDefs ~ _ => DataDictionary(columnDefs)}
  }

  def apply(tokens: Seq[DictionaryToken]): Either[DictionaryParserError, DataDictionary] = {
    val reader = new DictionaryTokenReader(tokens)
    dictionary(reader) match {
      case NoSuccess(msg, next) => Left(DictionaryParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
}
