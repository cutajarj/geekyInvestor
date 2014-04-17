package tools

import com.mongodb.MongoURI
import persistance.SymbolDAOImpl

case class Constituent(name: String, desc: String)

object SP500Scanner {

  val mongoDB = {
    val mongoURI = new MongoURI("mongodb://127.0.0.1:27017/geekInvestor")
    val db = mongoURI.connectDB()
    Option(mongoURI.getUsername).foreach(username => db.authenticate(username, mongoURI.getPassword))
    db
  }

  val symbolDAO = new SymbolDAOImpl(mongoDB)

  def main(args: Array[String]) = {
    val url = "http://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    val contents = io.Source.fromURL(url)("UTF-8").getLines
    val everything = contents.toList
    val start = everything.indexWhere(_.contains("<table"))
    val end = everything.indexWhere(_.contains("</table>"))

    val table = everything.slice(start + 1, end)
    val tableGrouped = table.foldLeft(List[String]("")) { (grouping, row) =>
      if (row.contains("</tr>")) {
        grouping :+ ""
      } else {
        grouping.take(grouping.size - 1) :+ (grouping.last + row)
      }
    }
    val wikiTickers = tableGrouped.tail.flatMap { group =>
      val uptoTicker = group.replace("<tr><td>", "").dropWhile(_ != '>').drop(1)
      val tickerName = uptoTicker.takeWhile(_ != '<').trim
      val uptoDesc = uptoTicker.dropWhile(_ != '<').drop(16).dropWhile(_ != '>').drop(1)
      val tickerDesc = uptoDesc.takeWhile(_ != '<').trim
      Option(tickerName).filterNot(_.isEmpty()).map(Constituent(_, tickerDesc))
    }
    println(s"All tickers loaded from wiki size ${wikiTickers.size}")
    val dbTickers = symbolDAO.loadAllStockTickers.filter(_.active=="A")
    println(s"All tickers loaded from db size ${dbTickers.size}")
    merge(wikiTickers, dbTickers)
  }

  def merge(wikiTickers: Seq[Constituent], dbTickers: Seq[model.Symbol]) {
    val wikiNames = wikiTickers.map(_.name).toSet
    val dbNames = dbTickers.map(_.name).toSet
    val wikiNamesToAdd = wikiNames -- dbNames
    val dbNamesToDisable = dbNames -- wikiNames
    val tickersToAdd = wikiNamesToAdd.flatMap { name =>
      wikiTickers.filter(_.name == name).map(t =>
        model.Symbol(name = t.name, desc = t.desc, remoteId = t.name, symbolType = "STOCK"))
    }
    val tickersToRemove = dbNamesToDisable.flatMap { name =>
      dbTickers.filter(_.name == name).map(_.copy(active = "D"))
    }

    println(s"wikiNamesToAdd: $tickersToAdd")
    println(s"dbNamesToDisable: $tickersToRemove")
    symbolDAO.saveAllSymbols(tickersToAdd)
    tickersToRemove.foreach(symbolDAO.updateSymbol(_))
  }

}