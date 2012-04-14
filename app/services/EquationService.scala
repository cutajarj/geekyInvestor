package services

import org.joda.time.LocalDate
import parser.{ECurrency, ExprParser, EStock, Expr}
import org.slf4j.LoggerFactory
import EquationServiceImpl._
import collection.JavaConversions._
import java.util.{Date, TreeMap}
import java.text.{ParseException, SimpleDateFormat}

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 24/02/12
 * Time: 18:41
 * To change this template use File | Settings | File Templates.
 */

trait EquationService{
  def buildEquation(equation: String): Expr
  def workOut(expr: Expr, fromDate: LocalDate, toDate: LocalDate): EquationResult
}

object EquationServiceImpl {
  val LOG = LoggerFactory.getLogger(classOf[EquationServiceImpl])
}

case class EquationResult(success: Boolean, messages: Seq[String], result: Iterator[(Double, Double)])

class EquationServiceImpl(val equationParser: ExprParser,val statService: StatService) extends EquationService{

  def buildEquation(equation: String): Expr = {
    LOG.info("Parsing equation {}", equation)
    equationParser.apply(equation)
  }

  def workOut(expr: Expr, fromDate: LocalDate, toDate: LocalDate): EquationResult = {
    LOG.info("Working out {} ", expr)
    try {
      //Extract all symbols
      val stock = expr.leaves.collect {
        case x: EStock => x
      }.toSet
      val currencies = expr.leaves.collect {
        case x: ECurrency => x
      }.toSet

      val timeStartMaps = System.currentTimeMillis

      //Load types in a map symbol.type->Tree[Date->Stat]
      val statsStockMap = statService.buildStockStatsMap(stock, fromDate, toDate)
      val statsCurrencyMap = statService.buildCurrencyStatsMap(currencies, fromDate, toDate)
      LOG.info("Time taken for eq {} to build all maps {}", expr, System.currentTimeMillis() - timeStartMaps)
      val timeStartMapping = System.currentTimeMillis

      //Find shortest one and adjust to and from dates
      val smallest = smallestTree(statsStockMap ++ statsCurrencyMap)
      val truncDateFrom = smallest.firstKey

      //Find Tree[Date->Stat] with biggest size
      val biggestStats = largestTree(statsStockMap ++ statsCurrencyMap)

      val msgTruncation = if (biggestStats.firstKey() < truncDateFrom) {
        val sdf = new SimpleDateFormat("dd MMM yyyy")
        Seq[String]("Date range has been reduced as data for %s is only available from %s".format(
          smallestTreeSymbol(statsStockMap ++ statsCurrencyMap), sdf.format(new Date(truncDateFrom))))
      } else {
        Seq[String]()
      }

      //Iterate through it building a map for symbols and evaluating the equation
      val keys = biggestStats.navigableKeySet().iterator
      val results = keys.collect {
        case currentDate if (currentDate > truncDateFrom) =>
          val paramsStock = createParamsFor(currentDate, statsStockMap)
          val paramsCurrency = createParamsFor(currentDate, statsCurrencyMap)
          val result = expr.eval(paramsCurrency ++ paramsStock)
          (currentDate.toDouble, result)
      }
      LOG.info("Time taken for eq {} to finish mapping {}", expr, System.currentTimeMillis() - timeStartMapping)
      EquationResult(true, msgTruncation, results)

    }
    catch {
      case e: ParseException =>
        LOG.error("Error while generating %s %s %s".format(expr, fromDate, toDate), e)
        EquationResult(false, Seq[String]("An unexpected error has occurred... Woops!"), Iterator())
      case e: IllegalArgumentException =>
        EquationResult(false, Seq[String](e.getMessage), Iterator())
      case e =>
        LOG.error("Error while generating %s %s %s".format(expr, fromDate, toDate), e)
        EquationResult(false, Seq[String]("An unexpected error has occurred... Woops!"), Iterator())
    }
  }

  private[this] def smallestTree(statsMap: Map[String, TreeMap[Long, Double]]) =
    statsMap.minBy(v => v._2.size)._2

  private[this] def smallestTreeSymbol(statsMap: Map[String, TreeMap[Long, Double]]) =
    statsMap.minBy(v => v._2.size)._1

  private[this] def largestTree(statsMap: Map[String, TreeMap[Long, Double]]) =
    statsMap.maxBy(v => v._2.size)._2

  private[this] def createParamsFor(currentDate: Long, symbolsToStats: Map[String, TreeMap[Long, Double]]) =
    symbolsToStats.map {
      case (i, v) =>
        (i, v.floorEntry(currentDate).getValue)
    }.toMap
}
