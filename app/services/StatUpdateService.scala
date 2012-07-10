package services

import org.slf4j.LoggerFactory
import persistance.{SymbolDAO, StatDAO}
import model._
import java.net.{HttpURLConnection, URL}
import java.util.Date
import org.joda.time.{Days, LocalDate}
import collection.immutable.{TreeMap, TreeSet}
import StatUpdateServiceImpl._
import util.matching.Regex
import java.text.SimpleDateFormat
import collection.JavaConversions._
import collection.mutable.ArrayBuffer

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
  val days10 = 1000*60*60*24*10 //less than this we overwrite the value with warning
  val days75 = 1000*60*60*24*75 //less than this we create new with warning
  val days105 = 1000*60*60*24*105 //More than this we create a new value with warning
}

case class StatsToUpdate(symbol: Symbol, mrq: Date, values: Map[SymbolType, Option[Double]])

class StatUpdateServiceImpl(val symbolDAO: SymbolDAO, val statDAO: StatDAO) extends StatUpdateService {

  val mostRecentQuarter = """Most Recent Quarter \(mrq\):</td><td class="yfnc_tabledata1">([\d|\w|\s|,]+)</td></tr>""".r
  val mrqDateFormat = new SimpleDateFormat("MMM dd, yyyy")

  val extractFundamentalMap = Map[String, Regex](
    "BVPS" -> """Book Value Per Share \(mrq\):</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r,
    "EPSTTM" -> """Diluted EPS \(ttm\):</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r,
    "RTTM" -> """Revenue \(ttm\):</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r,
    "RPSTTM" -> """Revenue Per Share \(ttm\):</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r,
    "SO" -> """Shares Outstanding<font size="-1"><sup>5</sup></font>:</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r,
    "GPTTM" -> """Gross Profit \(ttm\):</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r,
    "NITTM" -> """Net Income Avl to Common \(ttm\):</td><td class="yfnc_tabledata1">([,|\-|\d|\w|\.|/]+)</td></tr>""".r
  )

  def updateStock(updateLatest: Boolean): Seq[(String, Double, Double, LocalDate)] = {
    LOG.info("Starting stats update...")
    val allSymbols = symbolDAO.loadAllSymbols()
    LOG.info("Using {} symbols in total.", allSymbols.size)
    val allSymbolTypes = symbolDAO.loadAllFundamentalTypes()
    val setToUpdate = allSymbolTypes.filter(_.remoteId.isDefined).foldLeft(Set[Symbol]()) { (setToUpdate, t) =>
      val dataList = downloadData(allSymbols, t)
      setToUpdate ++ compareToStore(dataList, t, updateLatest)
    }
    LOG.info("Symbols needing updating are: {}", setToUpdate.map(_.name))
    val latestStats = downloadLatestStats(setToUpdate, allSymbolTypes)
    updateLocalStats(latestStats)
    Seq()
  }

  private[this] def updateLocalStats(latestStats:Set[StatsToUpdate]) = latestStats.foreach ( onlineStat => onlineStat.values.foreach { stValue =>
    val dbLatest = statDAO.loadLatest(stValue._1.name,onlineStat.symbol.name)
    val dbDate = LocalDate.fromDateFields(dbLatest.timeStamp)
    val mrqDate = LocalDate.fromDateFields(onlineStat.mrq)

    Days.daysBetween(dbDate, mrqDate).getDays match {
      case x if (x < 10) =>
        LOG.warn("Symbol {} type {} latest is less than 10 days from online's mrq. Overwritting value.",onlineStat.symbol.name,stValue._1.name)
        LOG.warn("DB timestamp, value, online mrq, value, tsdiff {}",Array(dbLatest.timeStamp,dbLatest.value,onlineStat.mrq,stValue._2,x))
        stValue._2.map( v=> statDAO.update(dbLatest.copy(timeStamp = onlineStat.mrq, value = v)) )
      case x if (x < 75) =>
        LOG.warn("Symbol {} type {} latest is less than 75 days from online's mrq. Creating new record.",onlineStat.symbol.name,stValue._1.name)
        LOG.warn("DB timestamp, value, online mrq, value, tsdiff {}",Array(dbLatest.timeStamp,dbLatest.value,onlineStat.mrq,stValue._2,x))
        stValue._2.map( v=> statDAO.save(dbLatest.copy(timeStamp = onlineStat.mrq, value = v)) )
      case x if (x > 105) =>
        LOG.warn("Symbol {} type {} latest is more than 105 days from online's mrq. Creating new record.",onlineStat.symbol.name,stValue._1.name)
        LOG.warn("DB timestamp, value, online mrq, value, tsdiff {}",Array(dbLatest.timeStamp,dbLatest.value,onlineStat.mrq,stValue._2,x))
        stValue._2.map( v=> statDAO.save (dbLatest.copy(timeStamp = onlineStat.mrq, value = v)) )
      case _ =>
        stValue._2.map( v=> statDAO.save(dbLatest.copy(timeStamp = onlineStat.mrq, value = v)) )
    }
  })

  private[this] def downloadLatestStats(setToUpdate: Set[Symbol], symbolTypes: Seq[SymbolType]) = setToUpdate.map { s =>
    val response = downloadEntirePage("http://finance.yahoo.com/q/ks?s=%s".format(s.remoteId))
    val extracted = mostRecentQuarter.findFirstMatchIn(response)
    val mrq = extracted match {
      case Some(mostRecentQuarter(dateStr)) => mrqDateFormat.parse(dateStr)
      case x => throw new IllegalStateException("MRQ not found or not in the right format (%s)".format(x))
    }

    val symbolValueMap = symbolTypes.map { st =>
      val regex = extractFundamentalMap(st.name)
      val extractedStr =  regex.findFirstMatchIn(response)
      val value = extractedStr match {
        case Some(regex(valueStr)) => st.remoteConversion(valueStr)
        case x => throw new IllegalStateException("%s not found or not in the right format (%s)".format(st.name,x))
      }
      (st->value)
    }.toMap
    val stu = StatsToUpdate(s,mrq,symbolValueMap)
    LOG.info(stu.toString)
    stu
  }

  private[this] def downloadEntirePage(urlStr: String) = {
    LOG.info("Downloading {}", urlStr)
    val url = new URL(urlStr)
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(60000)

    connection.getResponseCode match {
      case HttpURLConnection.HTTP_OK =>
        val bufferedSource = io.Source.fromInputStream(connection.getInputStream)
        bufferedSource.getLines().foldLeft("") { (s, l) => s + l}
      case _ =>
        throw new IllegalStateException("Cannot download from %s".format(urlStr))
    }
  }

  private[this] def downloadData(allSymbols: Seq[Symbol], t: SymbolType): Seq[(Symbol, Option[Double])] = {
    val headList = allSymbols.slice(0, math.min(80, allSymbols.size))
    val dataList = retreiveData(headList, t)
    if (allSymbols.size > 80)
      dataList ++ downloadData(allSymbols.slice(80, allSymbols.size), t)
    else
      dataList
  }

  private[this] def compareToStore(dataList: Seq[(Symbol, Option[Double])], t: SymbolType, updateLatest: Boolean) = dataList.flatMap { x =>
    val stat = statDAO.loadLatest(t.name, x._1.name)
    x._2 match {
      case Some(v) if (math.abs(stat.value - v) > precision) => Some(x._1)
      case _ => None
    }
  }

  /**
   *
  if (updateLatest) {
            statDAO.update(stat.copy(value = x._2))
            Some(("%s.%s".format(x._1.name, t.name), stat.value, x._2, LocalDate.fromDateFields(stat.timeStamp) ))
          }
          else {
            val d = deriveLatestQuarter
            statDAO.save(stat.copy(value = x._2, timeStamp = d.toDate))
            Some(("%s.%s".format(x._1.name, t.name), stat.value, x._2, d))
          }
   */

  private[this] def retreiveData(symbols: Seq[Symbol], t: SymbolType): Seq[(Symbol, Option[Double])] = {
    val str = symbols.map(_.remoteId).reduceLeft((a, b) => a + "+" + b)
    val it = download("http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s".format(str, t.remoteId.get), t.remoteConversion)
    symbols.foldLeft(Seq[(Symbol, Option[Double])]()) { (m, s) =>
      try {
        m :+(s, it.next)
      } catch {
        case e: Exception => throw new IllegalStateException("Symbol %s returned invalid data (%s)".format(s.remoteId, e.getMessage), e)
      }
    }
  }

  private[this] def download(urlStr: String, f: String => Option[Double]) = {
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
