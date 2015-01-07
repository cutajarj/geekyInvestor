package tools

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

case class XbrlDetails(companyName: String, form: String, date: LocalDate, pathToTxt: String, pathId: String)

object EDGARIndexParser {
  
  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd")
  val TxtPattern = "(\\w*/\\w*/\\w*/)(.*)\\.txt".r

  def parseIndex(indexStr: Seq[String]) = indexStr.foldLeft(Map[Int, XbrlDetails]()) { (index, line) =>
    val lineSplit = line.split("\\|")
    val companyId = lineSplit(0).toInt
    val companyName = lineSplit(1)
    val form = lineSplit(2)
    val date = dateTimeFormat.parseLocalDate(lineSplit(3))
    val pathAndId = lineSplit(4) match {
      case TxtPattern(path, id) => (path, id)
      case _ => throw new IllegalStateException(s"Cannot parse index path ${lineSplit(4)}")
    }
    
    index + (companyId -> XbrlDetails(companyName, form, date, pathAndId._1, pathAndId._2))
  }
}