package persistance

import model.Symbol
import model.SymbolType
import org.slf4j.LoggerFactory
import com.mongodb.DB
import SymbolDAOImpl._
import collection.JavaConversions._
import com.mongodb.DBObject
import com.mongodb.BasicDBObject
import org.bson.types.ObjectId

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 02/04/12
 * Time: 18:19
 * To change this template use File | Settings | File Templates.
 */

trait SymbolDAO {
  def loadAllFundamentalTypes(): Seq[SymbolType]
  def loadAllTradedTypes(): Seq[SymbolType]
  def loadAllStockTickers(): Seq[Symbol]
  def loadAllCurrencies(): Seq[Symbol]
  def loadAllIndexes(): Seq[Symbol]
  def saveAllSymbols(symbols: Iterable[Symbol])
  def updateSymbol(stockTicker: Symbol)
}

object SymbolDAOImpl {
  val LOG = LoggerFactory.getLogger(classOf[SymbolDAOImpl])
}

class SymbolDAOImpl(val mongoDB: DB) extends SymbolDAO {

  override def loadAllFundamentalTypes(): Seq[SymbolType] = {
    Seq[SymbolType](
      SymbolType("BVPS", "Book Value per share", Some("b4"), "STOCK", mdDoubleConvertor),
      SymbolType("EPSTTM", "Earnings per share TTM", Some("e"), "STOCK", mdDoubleConvertor),
      SymbolType("RTTM", "Revenue TTM", Some("s6"), "STOCK", mdDoubleConvertor),
      SymbolType("RPSTTM", "Revenue per share TTM", None, "STOCK", mdDoubleConvertor),
      SymbolType("GPTTM", "Gross profit TTM", None, "STOCK", mdDoubleConvertor),
      SymbolType("NITTM", "Net Income TTM", None, "STOCK", mdDoubleConvertor),
      SymbolType("SO", "Shares Outstanding", Some("j2"), "STOCK", mdDoubleConvertor))
  }

  override def loadAllCurrencies(): Seq[Symbol] = {
    Seq[Symbol](
      Symbol("USD", "US dollar", "USD", "CURRENCY"),
      Symbol("GBP", "British pound", "GBP", "CURRENCY"),
      Symbol("CNY", "Chinese yuan", "CNY", "CURRENCY"),
      Symbol("BRL", "Brazil real", "BRL", "CURRENCY"),
      Symbol("EUR", "Euro", "EUR", "CURRENCY"))

  }

  override def loadAllIndexes(): Seq[Symbol] = {
    Seq[Symbol](
      Symbol("^GSPC", "S&P 500 Index", "^GSPC", "INDEX"),
      Symbol("^VIX", "VIX Index", "^VIX", "INDEX"),
      Symbol("^DJI", "Dow Jones Industrial Average", "^DJI", "INDEX"),
      Symbol("^DJT", "Dow Jones Transporatation Average", "^DJT", "INDEX"),
      Symbol("^DJU", "Dow Jones Utility Average", "^DJU", "INDEX"),
      Symbol("^NDX", "NASDAQ 100", "^NDX", "INDEX"),
      Symbol("^IXIC", "NASDAQ Composite", "^IXIC", "INDEX"))
  }

  override def loadAllTradedTypes(): Seq[SymbolType] = {
    Seq[SymbolType](
      SymbolType("OPEN", "Opening Price", Some("OPEN"), "TRADED", commaDoubleConvertor),
      SymbolType("CLOSE", "Closing Price", Some("CLOSE"), "TRADED", commaDoubleConvertor),
      SymbolType("HIGH", "High Day Price", Some("HIGH"), "TRADED", commaDoubleConvertor),
      SymbolType("LOW", "Low Day Price", Some("LOW"), "STOCK", commaDoubleConvertor),
      SymbolType("VOLUME", "Volume Day", Some("VOLUME"), "STOCK", commaDoubleConvertor))
  }

  def commaDoubleConvertor(s: String): Option[Double] = s match {
    case "N/A" => None
    case x => Some(x.replace(",", "").toDouble)
  }

  def mdDoubleConvertor(s: String): Option[Double] = {
    val bPattern = """(-?[0-9|,]+(\.[0-9][0-9]*)?)B""".r
    val mPattern = """(-?[0-9|,]+(\.[0-9][0-9]*)?)M""".r
    val sPattern = """(-?[0-9|,]+(\.[0-9][0-9]*)?)""".r

    s.trim match {
      case bPattern(x, y) => Some(x.replace(",", "").toDouble * 1000000000.0)
      case mPattern(x, y) => Some(x.replace(",", "").toDouble * 1000000.0)
      case sPattern(x, y) => Some(x.replace(",", "").toDouble)
      case "N/A" => None
      case _ => throw new IllegalArgumentException("Invalid decimal format: " + s)
    }
  }

  override def loadAllStockTickers(): Seq[Symbol] = {
    LOG.debug("Loading all tickers")
    val stockTickerColl = mongoDB.getCollection("stockTickerColl")
    val results: Iterable[DBObject] = stockTickerColl.find()
    val symbols = results.map { entity =>
      Symbol(key = Some(entity.get("_id").toString),
        name = entity.get("n").toString,
        desc = entity.get("d").toString,
        remoteId = entity.get("rid").toString,
        symbolType = entity.get("st").toString,
        active = entity.get("a").toString)
    }
    symbols.toSeq
  }

  override def saveAllSymbols(stockTickers: Iterable[Symbol]) {
    LOG.debug("Saving all tickers")

    val tickersToInsert = stockTickers.map { symbol =>
      val entity = new BasicDBObject()
      entity.put("n", symbol.name)
      entity.put("d", symbol.desc)
      entity.put("rid", symbol.remoteId)
      entity.put("st", symbol.symbolType)
      entity.put("a", symbol.active)
      entity.asInstanceOf[DBObject]
    }.toList

    val stockTickerColl = mongoDB.getCollection("stockTickerColl")
    stockTickerColl.insert(tickersToInsert)
  }

  override def updateSymbol(stockTicker: Symbol) = stockTicker.key.map { k =>
    LOG.debug("Updating {}", stockTicker)
    val stockTickerColl = mongoDB.getCollection("stockTickerColl")
    val updateCriteria = new BasicDBObject()
    updateCriteria.append("_id", new ObjectId(k))

    val entity = new BasicDBObject()
    entity.put("n", stockTicker.name)
    entity.put("d", stockTicker.desc)
    entity.put("rid", stockTicker.remoteId)
    entity.put("st", stockTicker.symbolType)
    entity.put("a", stockTicker.active)
    entity.put("_id", new ObjectId(k))

    stockTickerColl.update(updateCriteria, entity)
  }
}
