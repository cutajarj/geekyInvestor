package controllers

import play.api._
import data._
import data.Forms._
import mvc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import config.AppConfig._
import org.slf4j.LoggerFactory

object Application extends Controller {
  val LOG = LoggerFactory.getLogger(classOf[Controller])

  case class ChartRequest(equation: String, yColorValue: String, dateFrom: LocalDate, dateTo: LocalDate, title: String)

  def index = Action {
    Ok(views.html.index("The Wall Street Nerd", chartForm.fill(ChartRequest("", "DD1111", LocalDate.now().minusYears(2), LocalDate.now(), ""))))
  }

  private[this] def isValidDate(strDate: String): Boolean = try {
    LocalDate.parse(strDate, DateTimeFormat.forPattern("MM/dd/yyyy"))
    true
  }
  catch {
    case _ => false
  }

  val chartForm = Form(
    mapping(
      "equation" -> text,
      "yColorValue" -> text,
      "dateFrom" -> text.verifying("Invalid From Date", {
        isValidDate(_)
      }),
      "dateTo" -> text.verifying("Invalid To Date", {
        isValidDate(_)
      }),
      "title" -> text
    )
      (
        (equation, yColorValue, dateFrom, dateTo, title) => ChartRequest(equation, yColorValue,
          LocalDate.parse(dateFrom, DateTimeFormat.forPattern("MM/dd/yyyy")),
          LocalDate.parse(dateTo, DateTimeFormat.forPattern("MM/dd/yyyy")), title)
      )
      (
        (chartRequest: ChartRequest) => Some((chartRequest.equation, chartRequest.yColorValue,
          chartRequest.dateFrom.toString(DateTimeFormat.forPattern("MM/dd/yyyy")),
          chartRequest.dateTo.toString(DateTimeFormat.forPattern("MM/dd/yyyy")), chartRequest.title))
      ).verifying("From date must be before To date", {
      chartRequest => chartRequest.dateFrom.isBefore(chartRequest.dateTo)
    })
  )


  def chart = Action { implicit request =>
      chartForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.index("The Wall Street Nerd", formWithErrors)),
        chartRequest => {
          LOG.debug("Equation: {}", chartRequest.equation)
          try {
            val eq = equationService.buildEquation(chartRequest.equation)
            val eqResult = equationService.workOut(eq, chartRequest.dateFrom, chartRequest.dateTo)
            Ok(views.html.chart("The Wall Street Nerd %s".format(chartRequest.title), chartForm.fill(chartRequest), eqResult, eqResult.messages))
          }
          catch {
            case e: IllegalArgumentException =>
              LOG.error("Error: {}", e.getMessage)
              BadRequest(views.html.index("The Wall Street Nerd", chartForm.fill(chartRequest), errorMessages = Seq[String](e.getMessage)))
          }
        }
    )
  }

}
