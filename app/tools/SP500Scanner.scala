package tools

import com.mongodb.MongoURI
import persistance.CompanyDAOImpl
import model.Company
import org.joda.time.LocalDate

case class Constituent(ric: String, name: String, cik: String)

object SP500Scanner {

  val mongoDB = {
    val mongoURI = new MongoURI("mongodb://127.0.0.1:27017/openFundamentals")
    val db = mongoURI.connectDB()
    Option(mongoURI.getUsername).foreach(username => db.authenticate(username, mongoURI.getPassword))
    db
  }

  val companyDAO = new CompanyDAOImpl(mongoDB)

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
    val wikiCompanies = tableGrouped.tail.flatMap { group =>
      val uptoTicker = group.replace("<tr><td>", "").dropWhile(_ != '>').drop(1)
      val tickerName = uptoTicker.takeWhile(_ != '<').trim
      val uptoDesc = uptoTicker.dropWhile(_ != '<').drop(16).dropWhile(_ != '>').drop(1)
      val tickerDesc = uptoDesc.takeWhile(_ != '<').trim
      val cik = uptoDesc.split("<td>").last.replace("</td>", "").trim
      Option(tickerName).filterNot(_.isEmpty()).map(Constituent(_, tickerDesc, cik))
    }
    println(s"All tickers loaded from wiki size ${wikiCompanies.size}")

    val dbCompanies = companyDAO.loadAllCompanies.filter(_.active == "A")
    println(s"All tickers loaded from db size ${dbCompanies.size}")
    merge(wikiCompanies, dbCompanies)
  }

  def merge(wikiCompanies: Seq[Constituent], dbCompanies: Seq[Company]) {
    val wikiRics = wikiCompanies.map(_.ric).toSet
    val dbRics = dbCompanies.map(_.ric).toSet
    val wikiRicsToAdd = wikiRics -- dbRics
    val dbRicsToDisable = dbRics -- wikiRics
    val companiesToAdd = wikiRicsToAdd.flatMap { ric =>
      wikiCompanies.filter(_.ric == ric).map(t =>
        Company(name = t.name, ric = t.ric, cik = Some(t.cik), lastFundumentalUpdate = new LocalDate(1970,1,1)))
    }.toSeq
    val companiesToRemove = dbRicsToDisable.flatMap { ric =>
      dbCompanies.filter(_.ric == ric).map(_.copy(active = "D"))
    }

    println(s"companiesToAdd: $companiesToAdd")
    println(s"companiesToDisable: $companiesToRemove")
    companyDAO.saveAllCompanies(companiesToAdd)
    companiesToRemove.foreach(companyDAO.updateCompany)
  }

}