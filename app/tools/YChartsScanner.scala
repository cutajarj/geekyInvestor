package tools

import java.text.SimpleDateFormat
import scala.xml.XML
import java.util.Date
import java.text.DecimalFormat
import java.io.{FileOutputStream, File, PrintWriter}
import persistance.{StatDAOImpl, SymbolDAOImpl, SymbolDAO}
import com.mongodb.{BasicDBObject, MongoURI}
import org.joda.time.LocalDate


/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 28/03/12
 * Time: 22:09
 * To change this template use File | Settings | File Templates.
 */

object YChartsScanner {
  def read(url: String) = io.Source.fromURL(url)

  val sdf = new SimpleDateFormat("MMM dd, yyyy")
  val sdf2 = new SimpleDateFormat("MM-dd-yyyy")
  val bPattern = """(-?[0-9]+(\.[0-9][0-9]*)?)B""".r
  val mPattern = """(-?[0-9]+(\.[0-9][0-9]*)?)M""".r
  val sPattern = """(-?[0-9]+(\.[0-9][0-9]*)?)""".r

  val mongoDB = {
    val mongoURI = new MongoURI(System.getProperty("MONGOHQ_URL"))
    val db = mongoURI.connectDB()
    Option(mongoURI.getUsername).foreach(username=>db.authenticate(username, mongoURI.getPassword))
    val index = new BasicDBObject()
    index.put("t", 1)
    index.put("s", 1)

    db.getCollection("fundamentalColl").ensureIndex(index)
    db
  }

  val statDAO = new StatDAOImpl(mongoDB)

  val months = Map[String, String]("Jan." -> "Jan", "Feb." -> "Feb", "March" -> "Mar", "April" -> "Apr", "May" -> "May", "June" -> "Jun", "July" -> "Jul",
    "Aug." -> "Aug", "Sept." -> "Sep", "Oct." -> "Oct", "Nov." -> "Nov", "Dec." -> "Dec")

  def readDataFor(sym: String, t: (String,String)) = {
    val dbLatest = statDAO.loadLatest(t._2,sym)
    val latestDate = LocalDate.fromDateFields(dbLatest.timeStamp).plusDays(1)
    val today = LocalDate.now()
    val url = "http://ycharts.com/companies/%s/historical_data/%s?start_month=%s&start_day=%s&start_year=%s&end_month=%s&end_day=%s&end_year=%s".format(sym, t._1,
      latestDate.getMonthOfYear,
      latestDate.getDayOfMonth,
      latestDate.getYear,
      today.getMonthOfYear,
      today.getDayOfMonth,
      today.getYear
      )
    println(url)
    val input = read(url)
    //val input = read("http://ycharts.com/companies/%s/historical_data/%s?start_month=4&start_day=3&start_year=2002&end_month=6&end_day=3&end_year=2012".format(sym, t))
    val stuff = input.getLines.foldLeft("") {
      (s, l) =>
        (s, l) match {
          case (s, l) if s.contains("</table>") => s
          case (s, l) if (l.contains("<table class=\"histDataTable\">") || s.length != 0) => s + l + "\n"
          case _ => s
        }
    }
    val nameValues = (XML.loadString(stuff) \\ "td").map(_.text)
    val nameValueMap = nameValues.foldLeft((Seq[(Date, Double)](), (""))) { (ms, c) =>
      (ms, c) match {
        case ((m, ""), c) => (m, c)
        case ((m, x), c) =>
          parseDouble(c) match {
            case Some(d) => (m :+(parseDate(x), d), "")
            case None => (m, "")
          }
      }
    }
    println(sym + "." + t + " " + nameValueMap._1)
    nameValueMap._1
  }

  def main(args: Array[String]) = {
    val pw = new PrintWriter(new FileOutputStream(new File("output.txt"), true))
    val ts = Seq[(String, String)](("eps_ttm", "EPSTTM"), ("book_value_per_share", "BVPS"), /*("shares_outstanding", "SO"),*/ ("revenues_ttm", "RTTM"),("net_income_ttm", "NITTM"), ("gross_profit_ttm", "GPTTM") /*,("revenue_per_share", "RPSTTM")*/)
    //val x = io.Source.fromFile("snp500.txt")
    val symbolDAO = new SymbolDAOImpl(mongoDB)
    val symbols = symbolDAO.loadAllStockTickers()
    symbols.foreach { s =>
      ts.foreach { t =>
        val listOfData = readDataFor(s.name, t)
        val df = new DecimalFormat("#.###")
        listOfData.foreach { data =>
          pw.println("%s,%s,%s,%s".format(s.name, t._2, df.format(data._2), sdf2.format(data._1)))
          pw.flush()
        }
      }
    }
    pw.close()
  }

  def parseDouble(d: String) = d match {
    case bPattern(x, y) => Some(x.toDouble * 1000000000.0)
    case mPattern(x, y) => Some(x.toDouble * 1000000.0)
    case sPattern(x, y) => Some(x.toDouble)
    case "" => None
    case _ => throw new IllegalArgumentException("Invalid decimal format: " + d)
  }

  def parseDate(date: String): Date = {
    val d = months.foldLeft(date) {
      (d, e) =>
        d.replace(e._1, e._2)
    }
    sdf.parse(d)
  }

}
