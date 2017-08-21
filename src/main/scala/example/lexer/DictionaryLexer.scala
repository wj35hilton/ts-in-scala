package example.lexer

import scala.util.parsing.combinator.{RegexParsers}

sealed trait DictionaryToken

case object OPEN_BRACKET extends DictionaryToken
case object CLOSE_BRACKET extends DictionaryToken
case object OPEN_PAREN extends DictionaryToken
case object CLOSE_PAREN extends DictionaryToken

case object INFILE_KW extends DictionaryToken
case object DICTIONARY_KW extends DictionaryToken
case object COLUMN_DES_KW extends DictionaryToken

case class FORMAT(length: Integer, spec: String) extends DictionaryToken
case class IDENTIFIER(str: String) extends DictionaryToken
case class INTEGER(n: Integer) extends DictionaryToken
case class QUOTED(str: String) extends DictionaryToken

case class DictionaryLexerError(msg: String)

object DictionaryLexer extends RegexParsers {

  def openBracket = "{"                               ^^ (_ => OPEN_BRACKET)
  def closeBracket = "}"                              ^^ (_ => CLOSE_BRACKET)
  def openParen = "("                                 ^^ (_ => OPEN_PAREN)
  def closeParen = ")"                                ^^ (_ => CLOSE_PAREN)

  def integer: Parser[INTEGER] = """[0-9]+""".r       ^^ ( v => INTEGER(v.toString.toInt) )
  def quoted: Parser[QUOTED] = "\"[^\"]*\"".r         ^^ ( str => QUOTED(str) )

  def format: Parser[FORMAT] = """%[0-9]+[sf]""".r    ^^ { fmt =>
    val pattern = "%([0-9]+)([a-z])".r
    val pattern(length, spec) = fmt
    FORMAT(length.toInt, spec)
  }

  def identifier: Parser[DictionaryToken] =
    """[a-zA-Z_][a-zA-Z0-9_]*""".r                    ^^ { str =>
      str match {
        case "infile" => INFILE_KW
        case "dictionary" => DICTIONARY_KW
        case "_column" => COLUMN_DES_KW
        case _ => IDENTIFIER(str)
      }
    }

  def tokens: Parser[List[DictionaryToken]] = {
    phrase(rep1(identifier | integer | format | quoted | openBracket | closeBracket | openParen | closeParen)) ^^ { tokens => tokens }
  }

  def apply(code: String): Either[DictionaryLexerError, List[DictionaryToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(DictionaryLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }
}
