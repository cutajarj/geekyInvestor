package parser

import scala.util.parsing.combinator.syntactical._


/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 20/02/12
 * Time: 21:36
 * To change this template use File | Settings | File Templates.
 */

  class ExprParser extends StandardTokenParsers {
  override val lexical = new ExprLexical
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")

  def value = numericLit ^^ {
    s => EConst(s.toDouble)
  }

  def variable = stringLit ^^ {
    s =>
    //CHF|JPY|AUD|CAD
      val currency = "EUR|GBP|USD|CNY|BRL"
      val cRegex = ("(%s)(%s)".format(currency, currency)).r
      val stockTypeRegex = "(\\w+)\\.(\\w+)".r
      val stockOnlyRegex = "(\\w+)".r
      s match {
        case cRegex(from, to) => ECurrency(from, to)
        case stockTypeRegex(st, ty) => EStock(st, ty)
        case stockOnlyRegex(st) => EStock(st)
      }
  }

  def parens: Parser[Expr] = "(" ~> expr <~ ")"

  def unaryMinus: Parser[EUMinus] = "-" ~> term ^^ {
    EUMinus(_)
  }

  def term = (value | variable | parens | unaryMinus)

  def binaryOp(level: Int): Parser[((Expr, Expr) => Expr)] = {
    level match {
      case 1 =>
        "+" ^^^ {
          (a: Expr, b: Expr) => EAdd(a, b)
        } |
          "-" ^^^ {
            (a: Expr, b: Expr) => ESub(a, b)
          }
      case 2 =>
        "*" ^^^ {
          (a: Expr, b: Expr) => EMul(a, b)
        } |
          "/" ^^^ {
            (a: Expr, b: Expr) => EDiv(a, b)
          }
      case _ => throw new RuntimeException("bad precedence level " + level)
    }
  }

  val minPrec = 1
  val maxPrec = 2

  def binary(level: Int): Parser[Expr] =
    if (level > maxPrec) term
    else binary(level + 1) * binaryOp(level)

  def expr = (binary(minPrec) | term)

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }

  def apply(s: String): Expr = {
    parse(s) match {
      case Success(tree, _) =>
        println(tree)
        tree
      case e: NoSuccess =>
        throw new IllegalArgumentException("Bad syntax: " + s)
    }
  }
}