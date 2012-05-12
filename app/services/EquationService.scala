package services

import org.joda.time.LocalDate
import parser.{ECurrency, ExprParser, EStock, Expr}
import org.slf4j.LoggerFactory
import EquationServiceImpl._
import collection.JavaConversions._
import java.util.{Date, TreeMap}
import java.text.{ParseException, SimpleDateFormat}
import model.{RebaseScale, SingleScale, ScaleType}


/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 24/02/12
 * Time: 18:41
 * To change this template use File | Settings | File Templates.
 */

trait EquationService {
  def buildEquation(equation: String): Expr

  def workOut(exprs: Seq[Expr], fromDate: LocalDate, toDate: LocalDate, scaleType: ScaleType = SingleScale): EquationResult
}

object EquationServiceImpl {
  val LOG = LoggerFactory.getLogger(classOf[EquationServiceImpl])
}

case class EquationResult(success: Boolean, messages: Seq[String], result: Seq[Iterator[(Double, Double)]])

class EquationServiceImpl(val equationParser: ExprParser, val statService: StatService) extends EquationService {

  override def buildEquation(equation: String): Expr = {
    LOG.info("Parsing equation {}", equation)
    equationParser.apply(equation)
  }

  override def workOut(exprs: Seq[Expr], fromDate: LocalDate, toDate: LocalDate, scaleType: ScaleType = SingleScale): EquationResult = {
    LOG.info("Working out {} ", exprs)
    try {
      //Extract all symbols
      val stock = exprs.map(_.leaves.collect {
        case x: EStock => x
      }).flatten.toSet

      val currencies = exprs.map(_.leaves.collect {
        case x: ECurrency => x
      }).flatten.toSet

      val timeStartMaps = System.currentTimeMillis

      //Load types in a map symbol.type->Tree[Date->Stat]
      val statsStockMap = statService.buildStockStatsMap(stock, fromDate, toDate)
      val statsCurrencyMap = statService.buildCurrencyStatsMap(currencies, fromDate, toDate)
      LOG.info("Time taken for eqs {} to collect all on/off line data {}", exprs, System.currentTimeMillis() - timeStartMaps)
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
      val results = scaleType match {
        case RebaseScale => exprs.map {expr =>
            val firstKeys = biggestStats.navigableKeySet().iterator.filter(_>truncDateFrom).slice(0,1)
            val keys = biggestStats.navigableKeySet().iterator
            val startTime = System.currentTimeMillis()
            val start = calculateResults(firstKeys, truncDateFrom, statsStockMap, statsCurrencyMap, expr)
            val r = calculateResultsR(keys, truncDateFrom, statsStockMap, statsCurrencyMap, expr,start.next._2)
            LOG.debug("Time taken for eq {} to calculate results {}", expr, System.currentTimeMillis() - startTime)
            LOG.debug("Size for eq {} is {}", expr)
            r
          }
        case _ =>  exprs.map {expr =>
          val keys = biggestStats.navigableKeySet().iterator
          val startTime = System.currentTimeMillis()
          val r = calculateResults(keys, truncDateFrom, statsStockMap, statsCurrencyMap, expr)
          LOG.debug("Time taken for eq {} to calculate results {}", expr, System.currentTimeMillis() - startTime)
          LOG.debug("Size for eq {} is {}", expr)
          r
        }
      }

      LOG.info("Time taken for all equations to finish mapping {}", System.currentTimeMillis() - timeStartMapping)
      EquationResult(true, msgTruncation, results)

    }
    catch {
      case e: ParseException =>
        LOG.error("Error while generating %s %s %s".format(exprs, fromDate, toDate), e)
        EquationResult(false, Seq[String]("An unexpected error has occurred. We'll be investigating this..."), Seq(Iterator()))
      case e: IllegalArgumentException =>
        EquationResult(false, Seq[String](e.getMessage), Seq(Iterator()))
      case e =>
        LOG.error("Error while generating %s %s %s".format(exprs, fromDate, toDate), e)
        EquationResult(false, Seq[String]("An unexpected error has occurred. We'll be investigating this..."), Seq(Iterator()))
    }
  }

  private[this] def calculateResultsR(keys: Iterator[Long], truncDateFrom: Long, statsStockMap: Map[String, TreeMap[Long, Double]],
                                     statsCurrencyMap: Map[String, TreeMap[Long, Double]], expr: Expr,start:Double) = keys.collect {
    case currentDate if (currentDate > truncDateFrom) =>
      val paramsStock = createParamsFor(currentDate, statsStockMap)
      val paramsCurrency = createParamsFor(currentDate, statsCurrencyMap)
      val result = expr.eval(paramsCurrency ++ paramsStock)
      (currentDate.toDouble, (result/start)*100.0)
  }

  private[this] def calculateResults(keys: Iterator[Long], truncDateFrom: Long, statsStockMap: Map[String, TreeMap[Long, Double]],
                                     statsCurrencyMap: Map[String, TreeMap[Long, Double]], expr: Expr) = keys.collect {
    case currentDate if (currentDate > truncDateFrom) =>
      val paramsStock = createParamsFor(currentDate, statsStockMap)
      val paramsCurrency = createParamsFor(currentDate, statsCurrencyMap)
      val result = expr.eval(paramsCurrency ++ paramsStock)
      (currentDate.toDouble, result)
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
