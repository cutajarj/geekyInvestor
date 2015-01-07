package tools

import scala.xml.Elem
import org.joda.time.format.DateTimeFormat
import model.Fundamental
import PartialFunction._
import org.joda.time.LocalDate

class EDGARFundamentalScanner(edgar: Elem, companyKey: String, edgarDate: LocalDate) {

  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd")

  /**
   * Diluted earnings per share
   */
  def earningsPerShare: Seq[Fundamental] =
    extractMultipleMetricSummingExplicitMembers(Seq("EarningsPerShareDiluted", "EarningsPerShareBasicAndDiluted", "EarningsPerShareBasic"), "EPS")

  /**
   * Number of shares
   */
  def sharesOutStanding: Seq[Fundamental] = {
    val so = extractMetric("CommonStockSharesOutstanding", "SO") match {
      case Nil =>
        val soWtContext = extractMetric("CommonStockSharesOutstanding", "SO", true)
        val soGroupedByDate = soWtContext.groupBy(_.toDate)
        val sums = soGroupedByDate.map { case (date, so) => so.reduce((a, b) => a.copy(value = a.value + b.value)) }
        sums.toSeq
      case x => x
    }
    so match {
      case Nil => extractMultipleMetric(Seq("EntityCommonStockSharesOutstanding", "WeightedAverageNumberOfDilutedSharesOutstanding"), "SO", true)
      case x => x
    }
  }

  /**
   * total money brought in
   */
  def revenues: Seq[Fundamental] =
    extractMultipleMetricSummingExplicitMembers(Seq("Revenues", "SalesRevenueNet", "SalesRevenueServicesGross", "SalesRevenueServicesNet", "SalesRevenueGoodsNet",
      "TotalRevenuesAndOtherIncome", "RevenuesAndOtherIncome", "TotalRevenuesNetOfInterestExpense", "RevenuesNetOfInterestExpense", "RevenueOilAndGasServices", 
      "RealEstateRevenueNet", "RevenuesExcludingCorporate", "HealthCareOrganizationRevenue", "SalesAndOtherOperatingRevenueIncludingSalesBasedTaxes",
      "RevenuesExcludingInterestAndDividends", "InsuranceServicesRevenue", "BrokerageCommissionsRevenue", "FoodAndBeverageRevenue", "RevenueMineralSales", 
      "OilAndGasRevenue", "Revenuefromtransactionswithotheroperatingsegmentsofsameentity", "RefiningAndMarketingRevenue", "RevenueIncludingFullyTaxableEquivalentAdjustment"), "R")

  private def extractMultipleMetricSummingExplicitMembers(metrics: Seq[String], fundName: String): Seq[Fundamental] = {
    val fundamentals = extractMultipleMetric(metrics, fundName, false) match {
      case Nil => extractMultipleMetric(metrics, fundName, true)
      case x => x
    }
    val fundamentalsGroupedByDate = fundamentals.groupBy(f => (f.fromDateOpt, f.toDate))
    val sums = fundamentalsGroupedByDate.map { case (date, so) => so.reduce((a, b) => a.copy(value = a.value + b.value)) }
    sums.toSeq
  }

  private def extractMultipleMetric(metrics: Seq[String], fundName: String, includeExplicitMembers: Boolean): Seq[Fundamental] = metrics match {
    case Nil => Nil
    case h :: t => extractMetric(h, fundName, includeExplicitMembers) match {
      case Nil => extractMultipleMetric(t, fundName, includeExplicitMembers)
      case x => x
    }
  }

  private def extractMetric(edgarStr: String, fundName: String, includeExplicitMembers: Boolean = false) = {
    val field = edgar \ edgarStr
    field.flatMap { r =>
      val contextRef = (r \ "@contextRef").text
      val periodContext = extractPeriodContext(contextRef)
      val amount = Option(r.text).filterNot(_.isEmpty).map(_.toDouble)
      amount.flatMap(a => condOpt(periodContext.explicitMembers, includeExplicitMembers) {
        case (Nil, false) | (_, true) => Fundamental(
          companyKey = companyKey,
          name = fundName,
          value = a,
          fromDateOpt = periodContext.start,
          toDate = periodContext.end,
          updatedDate = edgarDate)
      })
    }
  }

  private def extractPeriodContext(contextId: String) = {
    val context = (edgar \ "context").filter { x =>
      val id = x \ "@id"
      id.text == contextId
    }
    val startDateStr = Option((context \ "period" \ "startDate").text).filter(!_.isEmpty)
    val endDateStr = Option((context \ "period" \ "endDate").text).filter(!_.isEmpty)
    val instantdateStr = Option((context \ "period" \ "instant").text).filter(!_.isEmpty)
    val dateStr = endDateStr.orElse(instantdateStr).getOrElse(throw new IllegalStateException("endDate and instant Date not found"))

    val explicitMembers = (context \ "entity" \ "segment" \ "explicitMember").map(_.text)
    PeriodContext(startDateStr.map(dateTimeFormat.parseLocalDate), dateTimeFormat.parseLocalDate(dateStr), explicitMembers)
  }

}