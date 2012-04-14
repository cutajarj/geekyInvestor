package services

import org.slf4j.LoggerFactory
import persistance.{SymbolDAO, StatDAO}
import model._
import java.net.{HttpURLConnection, URL}
import java.util.Date
import org.joda.time.LocalDate
import collection.immutable.{TreeMap, TreeSet}
import StatUpdateServiceImpl._

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 04/04/12
 * Time: 20:26
 * To change this template use File | Settings | File Templates.
 */


trait StatUpdateService {
  def updateStock(updateLatest: Boolean): Seq[(String, Double, Double, LocalDate)]
}

object StatUpdateServiceImpl {
  val LOG = LoggerFactory.getLogger(classOf[StatUpdateServiceImpl])
  val precision = 0.00001
}

class StatUpdateServiceImpl(val symbolDAO: SymbolDAO,val statDAO: StatDAO) {

  def updateStock(updateLatest: Boolean): Seq[(String, Double, Double, LocalDate)] = {
    val allSymbols = symbolDAO.loadAllSymbols()
    val allSymbolTypes = symbolDAO.loadAllStockTypes()
    allSymbolTypes.foldLeft(Seq[(String, Double, Double, LocalDate)]()) {
      (l, t) =>
        val dataList = downloadData(allSymbols, t)
        l ++ updateStore(dataList, t, updateLatest)
    }
  }

  private[this] def downloadData(allSymbols: Seq[Symbol], t: SymbolType): Seq[(Symbol, Double)] = {
    val headList = allSymbols.slice(0, math.min(80, allSymbols.size))
    val dataList = retreiveData(headList, t)
    if (allSymbols.size > 80)
      dataList ++ downloadData(allSymbols.slice(80, allSymbols.size), t)
    else
      dataList
  }

  private[this] def updateStore(dataList: Seq[(Symbol, Double)], t: SymbolType, updateLatest: Boolean) = dataList.flatMap {
    x =>

      val stat = statDAO.loadLatest(t.name, x._1.name)
      math.abs(stat.value - x._2) match {
        case v if (v > precision) =>
          if (updateLatest) {
            statDAO.update(stat.copy(value = x._2))
            Some(("%s.%s".format(x._1.name, t.name), stat.value, x._2, LocalDate.fromDateFields(stat.timeStamp) ))
          }
          else {
            val d = deriveLatestQuarter
            statDAO.save(stat.copy(value = x._2, timeStamp = d.toDate))
            Some(("%s.%s".format(x._1.name, t.name), stat.value, x._2, d))
          }
        case v =>
          None
      }
  }

  private[this] def retreiveData(symbols: Seq[Symbol], t: SymbolType): Seq[(Symbol, Double)] = {
    val str = symbols.map(_.remoteId).reduceLeft((a, b) => a + "+" + b)
    val it = download("http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s".format(str, t.remoteId), t.remoteConversion)
    symbols.foldLeft(Seq[(Symbol, Double)]()) {
      (m, s) =>
        try {
          m :+(s, it.next)
        } catch {
          case e: Exception => throw new IllegalStateException("Symbol %s returned invalid data (%s)".format(s.remoteId, e.getMessage), e)
        }
    }
  }

  private[this] def download(urlStr: String, f: String => Double) = {
    LOG.info("GET {}", urlStr)
    val url = new URL(urlStr)
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(60000)

    connection.getResponseCode match {
      case HttpURLConnection.HTTP_OK =>
        val bufferedSource = io.Source.fromInputStream(connection.getInputStream)
        bufferedSource.getLines().collect {
          case x: String => f(x)
        }
      case _ =>
        throw new IllegalStateException("Cannot download from %s".format(urlStr))
    }
  }

  private[this] def deriveLatestQuarter(): LocalDate = {
    val now = LocalDate.now()
    val quarterMonthCalendar = TreeMap[Int, LocalDate]() +(0 -> new LocalDate(now.year.get - 1, 12, 31),
      3 -> new LocalDate(now.year.get, 3, 31),
      6 -> new LocalDate(now.year.get, 6, 30),
      9 -> new LocalDate(now.year.get, 9, 30))
    quarterMonthCalendar.to(now.monthOfYear.get).last._2
  }

}
