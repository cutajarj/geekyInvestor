package tools

import scala.xml._
import model.Stat
import org.joda.time.format.DateTimeFormat
import org.joda.time.LocalDate
import PartialFunction._
import org.joda.time.Days

case class InstantContext(instant: LocalDate, explicitMembers: Seq[String])

case class PeriodContext(start: Option[LocalDate], end: LocalDate, explicitMembers: Seq[String]) {
  def daysBetween = start.map(s => Days.daysBetween(s, end).getDays)
}

object EDGARScanner {
  def main(args: Array[String]) {
    val edgar = XML.loadFile("C:/MSDE/cutajar/cat-20131231.xml")
    val scanner = new EDGARScanner(edgar, "cat")
    val sharesOutStanding = scanner.sharesOutStanding
    val revenuesTTM = scanner.revenuesTTM
    val netIncomeTTM = scanner.netIncomeTTM
    val costOfGoodsTTM = scanner.costOfGoodsTTM
    val grossProfitTTM = scanner.grossProfitTTM(revenuesTTM, costOfGoodsTTM)
    val earningPerShareTTM = scanner.earningPerShareTTM
    val revenuePerShareTTM = scanner.revenuePerShareTTM(revenuesTTM, sharesOutStanding)
    val bvPerShare = scanner.bookValuePerShare(sharesOutStanding)

    sharesOutStanding.foreach(println)
    println
    revenuesTTM.foreach(println)
    println
    netIncomeTTM.foreach(println)
    println
    costOfGoodsTTM.foreach(println)
    println
    grossProfitTTM.foreach(println)
    println
    earningPerShareTTM.foreach(println)
    println
    revenuePerShareTTM.foreach(println)
    println
    bvPerShare.foreach(println)

  }
}

class EDGARScanner(edgar: Elem, symbolKey: String) {

  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd")

  //(("eps_ttm", "EPSTTM"), ("book_value_per_share", "BVPS"), ("revenues_ttm", "RTTM"), ("net_income_ttm", "NITTM"), ("gross_profit_ttm", "GPTTM") /*,("revenue_per_share", "RPSTTM")*/ )

  /**
   * Diluted earnings per share
   */
  def earningPerShareTTM: Seq[Stat] = {
    extract12MonthMetric("EarningsPerShareDiluted", "EPSTTM")
  }

  /**
   * Number of shares
   */
  def sharesOutStanding: Seq[Stat] = {
    extractInstantMetric("CommonStockSharesOutstanding", "SO")
  }

  /**
   * Stockholders equity per share
   */
  def bookValuePerShare(sharesOutstanding: Seq[Stat]): Seq[Stat] = {
    val totalEquity = extractInstantMetric("StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest", "TE")
    totalEquity.flatMap { te =>
      val shares = sharesOutstanding.find(_.timeStamp.equals(te.timeStamp))
      val bvPerShare = shares.map(te.value / _.value)
      bvPerShare.map(x => Stat(value = x, timeStamp = te.timeStamp, statType = "BVPS", symbol = symbolKey))
    }
  }

  /**
   * total money brought in
   */
  def revenuesTTM: Seq[Stat] = {
    extract12MonthMetric("Revenues", "RTTM")
  }

  /**
   * profit made after taxes and everything else has been paid
   */
  def netIncomeTTM: Seq[Stat] = {
    extract12MonthMetric("NetIncomeLossAvailableToCommonStockholdersBasic", "NITTM")
  }

  /**
   * profit made after subtracting the cost of making the product or providing the service, but not deducting payroll, taxation, interest
   */
  def grossProfitTTM(revenue: Seq[Stat], costOfGoods: Seq[Stat]): Seq[Stat] = {
    revenue.flatMap { rev =>
      val cost = costOfGoods.find(_.timeStamp.equals(rev.timeStamp))
      val grossProfit = cost.map(rev.value - _.value)
      grossProfit.map(x => Stat(value = x, timeStamp = rev.timeStamp, statType = "GPTTM", symbol = symbolKey))
    }
  }

  /**
   * costs to produce goods or service (raw materials not actual labour)
   */
  def costOfGoodsTTM: Seq[Stat] = {
    extract12MonthMetric("CostOfGoodsSold", "COGTTM")
  }

  /**
   * total money brought in dev shares outstanding
   */
  def revenuePerShareTTM(revenue: Seq[Stat], sharesOutstanding: Seq[Stat]): Seq[Stat] = {
    revenue.flatMap { rev =>
      val shares = sharesOutstanding.find(_.timeStamp.equals(rev.timeStamp))
      val revPerShare = shares.map(rev.value / _.value)
      revPerShare.map(x => Stat(value = x, timeStamp = rev.timeStamp, statType = "RPSTTM", symbol = symbolKey))
    }
  }

  private def extract12MonthMetric(edgarStr: String, statType: String) = {
    val field = edgar \ edgarStr
    field.flatMap { r =>
      val contextRef = (r \ "@contextRef").text
      val periodContext = extractPeriodContext(contextRef)
      val amount = r.text.toDouble
      condOpt(periodContext.explicitMembers, periodContext.daysBetween) {
        case (Nil, Some(d)) if (d > 360) => //Only TTM 365 days in a year
          Stat(value = amount, timeStamp = periodContext.end.toDate, statType = statType, symbol = symbolKey)
      }
    }
  }

  private def extractInstantMetric(edgarStr: String, statType: String) = {
    val field = edgar \ edgarStr
    field.flatMap { so =>
      val contextRef = (so \ "@contextRef").text
      val instantContext = extractInstantContext(contextRef)
      val amount = so.text.toDouble
      condOpt(instantContext.explicitMembers) {
        case Nil =>
          Stat(value = amount, timeStamp = instantContext.instant.toDate, statType = statType, symbol = symbolKey)
      }
    }
  }

  private def extractInstantContext(contextId: String) = {
    val context = (edgar \ "context").filter { x =>
      val id = x \ "@id"
      id.text == contextId
    }
    val dateStr = (context \ "period" \ "instant").text
    val explicitMembers = (context \ "entity" \ "segment" \ "explicitMember").map(_.text)
    InstantContext(dateTimeFormat.parseLocalDate(dateStr), explicitMembers)
  }

  private def extractPeriodContext(contextId: String) = {
    val context = (edgar \ "context").filter { x =>
      val id = x \ "@id"
      id.text == contextId
    }
    val startDateStr = (context \ "period" \ "startDate").text
    val endDateStr = (context \ "period" \ "endDate").text
    val explicitMembers = (context \ "entity" \ "segment" \ "explicitMember").map(_.text)
    PeriodContext(Some(dateTimeFormat.parseLocalDate(startDateStr)), dateTimeFormat.parseLocalDate(endDateStr), explicitMembers)
  }

}