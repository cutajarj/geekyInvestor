package services

import persistance.StatDAO
import model.Stat
import java.io.{BufferedReader, InputStreamReader}
import org.joda.time.LocalDate
import java.text.SimpleDateFormat
import java.util.TreeMap
import parser.{ECurrency, EStock, ExprParser}
import org.slf4j.LoggerFactory
import StatServiceImpl._
import java.util.Date
import java.net.{HttpURLConnection, URL}

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 12/02/12
 * Time: 16:26
 * To change this template use File | Settings | File Templates.
 */

trait StatService{
  def updateTodaysStats(): Seq[Stat]
  def buildStockStatsMap(stocks: Set[EStock], fromDate: LocalDate, toDate: LocalDate): Map[String, TreeMap[Long, Double]]
  def buildCurrencyStatsMap(cs: Set[ECurrency], fromDate: LocalDate, toDate: LocalDate): Map[String, TreeMap[Long, Double]]
}

object StatServiceImpl {
  val LOG = LoggerFactory.getLogger(classOf[StatService])
}

class StatServiceImpl(val statDAO: StatDAO,val equationParser: ExprParser) extends StatService{

  val symbolList = List[String]("RIO.L", "GOOG", "CCL.L", "VOD.L")
  val typesOnline = List[String]("ADJCLOSE", "CLOSE", "OPEN", "HIGH", "LOW")

  override def updateTodaysStats(): Seq[Stat] = {
    LOG.info("updating stats")
    val symbolString = symbolList.reduceLeft((l, s) => l + "," + s)
    val url = new URL("http://download.finance.yahoo.com/d/quotes.csv?s=%s&f=se&e=.csv".format(symbolString))

    val bufferedSource = io.Source.fromURL(url)
    val stats = bufferedSource.getLines().map {
      line =>
        println(line)
        line.split(",").toList match {
          case s :: e :: Nil => Stat(value = e.toDouble, timeStamp = new Date, statType = "EPS", symbol = s)
          case _ =>
            LOG.error("fail parsing...")
            throw new IllegalStateException("Failed to parse")
        }
    }

    stats.toSeq
  }

  override def buildStockStatsMap(stocks: Set[EStock], fromDate: LocalDate, toDate: LocalDate): Map[String, TreeMap[Long, Double]] = {
    val offline = extractOfflineSymbols(stocks)
    val online = extractOnlineSymbols(stocks)
    val offlineMap = loadOfflineSymbols(offline, fromDate, toDate)
    val onlineMap = loadOnlineSymbols(online, fromDate, toDate)

    offlineMap ++ onlineMap
  }

  override def buildCurrencyStatsMap(cs: Set[ECurrency], fromDate: LocalDate, toDate: LocalDate): Map[String, TreeMap[Long, Double]]
  = cs.foldLeft(Map[String, TreeMap[Long, Double]]()) {
    (m, c) =>
      val sourceInBase = currencyToBase(c.from, fromDate, toDate)
      LOG.debug("sourceInBase's size: {}", sourceInBase.size)
      val baseInTarget = baseToCurrency(c.to, fromDate, toDate)
      LOG.debug("baseInTarget's size: {}", baseInTarget.size)

      //find largest
      val largest = if (sourceInBase.size > baseInTarget.size) sourceInBase else baseInTarget

      // multiply both and return
      val keyIterator = largest.navigableKeySet().iterator
      val t = new TreeMap[Long, Double]()
      while (keyIterator.hasNext) {
        val k = keyIterator.next
        val c1 = Option(sourceInBase.floorEntry(k))
        val c2 = Option(baseInTarget.floorEntry(k))
        (c1, c2) match {
          case (Some(x), Some(y)) => t.put(k, x.getValue * y.getValue)
          case _ =>
        }
      }

      m + (c.id -> t)
  }

  private[this] def currencyToBase(currency: String, fromDate: LocalDate, toDate: LocalDate): TreeMap[Long, Double] = currency match {
    case "EUR" =>
      val t = new TreeMap[Long, Double]()
      t.put(fromDate.toDate.getTime, 1.0)
      t.put(toDate.toDate.getTime, 1.0)
      t
    case x =>
      readOnlineSymbol("EUR%s=X".format(x), "CLOSE", fromDate, toDate, true)
  }

  private[this] def baseToCurrency(currency: String, fromDate: LocalDate, toDate: LocalDate): TreeMap[Long, Double] = currency match {
    case "EUR" =>
      val t = new TreeMap[Long, Double]()
      t.put(fromDate.toDate.getTime, 1.0)
      t.put(toDate.toDate.getTime, 1.0)
      t
    case x =>
      readOnlineSymbol("EUR%s=X".format(x), "CLOSE", fromDate, toDate)
  }

  private[this] def loadOnlineSymbols(sym: Set[EStock], from: LocalDate, to: LocalDate): Map[String, TreeMap[Long, Double]] = sym.map {
    s =>
      (s.id, readOnlineSymbol(s.symbol, s.statType, from, to))
  }.toMap

  private[this] def readOnlineSymbol(sy: String, st: String, from: LocalDate, to: LocalDate, invert: Boolean = false): TreeMap[Long, Double] = {
    val stats = readOnlinePrices(sy, st, from, to)
    val t = new TreeMap[Long, Double]()
    stats.foreach(s => t.put(s.timeStamp.getTime, (if (invert) 1.0 / s.value else s.value)))
    t
  }

  private[this] def loadOfflineSymbols(sym: Set[EStock], from: LocalDate, to: LocalDate): Map[String, TreeMap[Long, Double]] = sym.map {
    s =>
      val stats = statDAO.load(Some(s.statType), Some(s.symbol))
      val t = new TreeMap[Long, Double]()
      stats.foreach(x => t.put(x.timeStamp.getTime, x.value))
      (s.id, t)
  }.toMap

  private[this] def extractOfflineSymbols(sym: Set[EStock]) = sym.filter {
    s => !typesOnline.exists(_ == s.statType)
  }

  private[this] def extractOnlineSymbols(sym: Set[EStock]) = sym.filter {
    s => typesOnline.exists(_ == s.statType)
  }

  private[this] def readOnlinePrices(symbol: String, statType: String, from: LocalDate, to: LocalDate): Iterator[Stat] = {
    println("Checking historical prices from yahoo")
    val urlStr = "http://ichart.yahoo.com/table.csv?s=%s&a=%s&b=%s&c=%s&d=%s&e=%s&f=%s&g=d&ignore=.csv".format(
      symbol,
      from.getMonthOfYear - 1,
      from.getDayOfMonth,
      from.getYear,
      to.getMonthOfYear - 1,
      to.getDayOfMonth,
      to.getYear
    )
    println("Getting %s", urlStr)
    val url = new URL(urlStr)
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(60000)

    connection.getResponseCode match {
      case HttpURLConnection.HTTP_OK =>
        val bufferedSource = io.Source.fromInputStream(connection.getInputStream)
        val sdf = new SimpleDateFormat("yyyy-MM-dd")
        bufferedSource.getLines().drop(1).flatMap { line =>
          line.split(",").toList match {
            case d :: o :: h :: l :: c :: v :: cAdj :: Nil =>
              try {
                Some(Stat(value = cAdj.toDouble, timeStamp = sdf.parse(d), statType = "ADJCLOSE", symbol = symbol))
              }
              catch {
                case e => throw new IllegalStateException(e)
              }
            case _ =>
              None
          }
        }
      case _ =>
        throw new IllegalArgumentException("Cannot find symbol %s".format(symbol))
    }
  }


}
