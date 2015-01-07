package tools

import persistance.CompanyDAOImpl
import com.mongodb.MongoURI
import com.mongodb.DB
import persistance.CompanyDAO
import org.joda.time.LocalDate
import java.net.URL
import java.net.HttpURLConnection
import java.io.ByteArrayOutputStream
import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.util.zip.ZipInputStream
import scala.xml._
import model.Company
import java.io.File
import java.io.FileOutputStream
import scala.util.Random
import scala.PartialFunction._

object EDGARLoader {
  val mongoDB = {
    val mongoURI = new MongoURI("mongodb://127.0.0.1:27017/openFundamentals")
    val db = mongoURI.connectDB()
    Option(mongoURI.getUsername).foreach(username => db.authenticate(username, mongoURI.getPassword))
    db
  }

  val companyDAO = new CompanyDAOImpl(mongoDB)

  def main(args: Array[String]) {

    val lines = io.Source.fromFile("C:/MSDE/cutajar/xbrl.idx").getLines
    val indexLines = lines.dropWhile(!_.startsWith("--------")).drop(1).toSeq

    val index = EDGARIndexParser.parseIndex(indexLines)

    val edgarLoader = new EDGARLoader(index, companyDAO)
    edgarLoader.downloadFundumentals()
  }

}

class EDGARLoader(index: Map[Int, XbrlDetails], companyDAO: CompanyDAO) {

  def getXbrl(url: String, details: XbrlDetails, company: Company) = {
    /*val extraPath = details.pathId.replace("-", "")
    val pathAdjusted = details.pathToTxt.replace("-", "").replace(".txt", "") + extraPath + "/" + details.pathId + "-xbrl.zip"
    val url = new URL(s"http://www.sec.gov/Archives/$pathAdjusted")
    println(s"connecting ${url}")
    val uc = url.openConnection()
    val connection = uc.asInstanceOf[HttpURLConnection]
    val in = connection.getInputStream()
    val buffer: Array[Byte] = new Array[Byte](1024)
    val fileOut = new FileOutputStream("C:/MSDE/cutajar/xbrlArchive/" + details.pathId + "-xbrl.zip")
    val out = new BufferedOutputStream(fileOut)
    Iterator.continually(in.read(buffer)).takeWhile(_ != -1).foreach(n => out.write(buffer, 0, n))
    out.flush()
    out.close*/

    val file = new File(url)
    if (file.exists()) {
      val zin = new ZipInputStream(new FileInputStream(url))
      val zipEntry = zin.getNextEntry()
      val xbrlStuff = XML.load(zin)
      val scanner = new EDGARFundamentalScanner(xbrlStuff, company.ric, details.date)
      /*cond(scanner.sharesOutStanding) {
        case Nil => 
          println(s"Processing ${company.name} ${company.cik} ${url} ${zipEntry.getName} missing sharesoutstanding")
          false
      }*/
      cond(scanner.revenues) {
        case Nil => 
          println(s"Processing ${company.name} ${company.cik} ${url} ${zipEntry.getName} missing revenues")
          false
      }
      /*cond(scanner.earningsPerShare) {
        case Nil => 
          println(s"Processing ${company.name} ${company.cik} ${url} ${zipEntry.getName} missing earningsPerShare")
          false                
      }*/
    }
  }

  def downloadFundumentals() {
    val companies = companyDAO.loadAllCompanies
    companies.foreach { company =>
      for (
        cik <- company.cik;
        details <- index.get(cik.toInt)
      ) yield {
        getXbrl("C:/MSDE/cutajar/xbrlArchive/" + details.pathId + "-xbrl.zip", details, company)
      }
    }
  }
}