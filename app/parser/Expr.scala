package parser

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 20/02/12
 * Time: 21:32
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class Expr {
  def eval(statMap: Map[String, Double]): Double

  def leaves: List[Expr]

  def toString: String
}

sealed abstract class ExprLeftRight extends Expr {
  val left: Expr
  val right: Expr

  def leaves: List[Expr] = left.leaves ++ right.leaves
}

case class EConst(value: Double) extends Expr {
  def eval(statMap: Map[String, Double]): Double = value

  def leaves: List[Expr] = List[Expr](this)

  override def toString: String = value.toString
}

case class EAdd(left: Expr, right: Expr) extends ExprLeftRight {
  def eval(statMap: Map[String, Double]): Double = left.eval(statMap) + right.eval(statMap)

  override def toString: String = "%s + %s".format(left.toString, right.toString)
}

case class ESub(left: Expr, right: Expr) extends ExprLeftRight {
  def eval(statMap: Map[String, Double]): Double = left.eval(statMap) - right.eval(statMap)

  override def toString: String = "%s - %s".format(left.toString, right.toString)
}

case class EMul(left: Expr, right: Expr) extends ExprLeftRight {
  def eval(statMap: Map[String, Double]): Double = left.eval(statMap) * right.eval(statMap)

  override def toString: String = "%s * %s".format(left.toString, right.toString)
}

case class EDiv(left: Expr, right: Expr) extends ExprLeftRight {
  def eval(statMap: Map[String, Double]): Double = left.eval(statMap) / right.eval(statMap)

  override def toString: String = "%s / %s".format(left.toString, right.toString)
}

case class EUMinus(e: Expr) extends Expr {
  def eval(statMap: Map[String, Double]): Double = -e.eval(statMap)

  def leaves: List[Expr] = e.leaves

  override def toString: String = "-%s".format(e.toString)
}

case class EStock(symbol: String, statType: String = "ADJCLOSE") extends Expr {
  def eval(statMap: Map[String, Double]): Double = statMap.get(id).getOrElse(0)

  def leaves: List[Expr] = List[Expr](this)

  def id = symbol + statType

  override def toString: String = "%s%s".format(symbol, if (statType == "ADJCLOSE") "" else "." + statType)
}

case class ECurrency(from: String, to: String) extends Expr {
  def eval(statMap: Map[String, Double]): Double = statMap.get(id).getOrElse(0)

  def leaves: List[Expr] = List[Expr](this)

  def id = from + to

  override def toString: String = "%s%s".format(from, to)
}
