package parser

import scala.util.parsing.combinator.lexical.StdLexical


/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 20/02/12
 * Time: 21:32
 * To change this template use File | Settings | File Templates.
 */


class ExprLexical extends StdLexical {
  override def token: Parser[Token] = indexSymbolToken | symbolWithTypeToken | floatingToken | super.token

  def symbolWithTypeToken: Parser[Token] = rep1(upperCaseChar) ~ optSymbolType ^^ {
    case symbol ~ t => StringLit((symbol mkString "") :: t :: Nil mkString "")
  }

  def indexSymbolToken: Parser[Token] = '^' ~ rep1(upperCaseChar) ~ optSymbolType ^^ {
    case i ~ s ~ t => StringLit( i :: (s mkString "") :: t :: Nil mkString "")
  }

  def upperCaseChar = elem("upperCaseChar", ch => ch.isLetter && ch.isUpper)

  def floatingToken: Parser[Token] =
    rep1(digit) ~ optFraction ~ optExponent ^^ {
      case intPart ~ frac ~ exp => NumericLit(
        (intPart mkString "") :: frac :: exp :: Nil mkString "")
    }

  def chr(c: Char) = elem("", ch => ch == c)

  def sign = chr('+') | chr('-')

  def optSign = opt(sign) ^^ {
    case None => ""
    case Some(sign) => sign
  }

  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }

  def symbolType = '.' ~ rep1(upperCaseChar) ^^ {
    case dot ~ tt => dot :: (tt mkString "") :: Nil mkString ""
  }

  def optFraction = opt(fraction) ^^ {
    case None => ""
    case Some(fraction) => fraction
  }

  def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
    case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
  }

  def optExponent = opt(exponent) ^^ {
    case None => ""
    case Some(exponent) => exponent
  }

  def optSymbolType = opt(symbolType) ^^ {
    case None => ""
    case Some(symbolType) => symbolType
  }

}