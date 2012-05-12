package controllers

import play.api._
import libs.json.Json._
import data._
import data.Forms._
import mvc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import config.AppConfig._
import org.slf4j.LoggerFactory
import model.{ScaleType, SingleScale, ChartRequest}


object Application extends Controller {
  val LOG = LoggerFactory.getLogger(classOf[Application])

  def index = Action {
    Ok(views.html.index("The Wall Street Nerd", chartForm.fill(ChartRequest(Seq(), Seq(), SingleScale, LocalDate.now().minusYears(2), LocalDate.now(), ""))))
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
      "equation2" -> text,
      "equation3" -> text,
      "yColorValue" -> text,
      "yColorValue2" -> text,
      "yColorValue3" -> text,
      "scale" -> text,
      "dateFrom" -> text.verifying("Invalid From Date", {
        isValidDate(_)
      }),
      "dateTo" -> text.verifying("Invalid To Date", {
        isValidDate(_)
      }),
      "title" -> text
    )
      (
        (equation, equation2, equation3, yColorValue, yColorValue2, yColorValue3, scale,dateFrom, dateTo, title) =>
          ChartRequest(Seq(equation, equation2, equation3).filter(_.nonEmpty),
            Seq(yColorValue, yColorValue2, yColorValue3).filter(_.nonEmpty),
            ScaleType.fromName(scale),
            LocalDate.parse(dateFrom, DateTimeFormat.forPattern("MM/dd/yyyy")),
            LocalDate.parse(dateTo, DateTimeFormat.forPattern("MM/dd/yyyy")), title)
      )
      (
        (chartRequest: ChartRequest) =>
          Some((chartRequest.equation.lift(0).getOrElse(""),
            chartRequest.equation.lift(1).getOrElse(""),
            chartRequest.equation.lift(2).getOrElse(""),
            chartRequest.yColorValue.lift(0).getOrElse("DD1111"),
            chartRequest.yColorValue.lift(1).getOrElse("F57C31"),
            chartRequest.yColorValue.lift(2).getOrElse("51CCF5"),
            chartRequest.scale.toString,
            chartRequest.dateFrom.toString(DateTimeFormat.forPattern("MM/dd/yyyy")),
            chartRequest.dateTo.toString(DateTimeFormat.forPattern("MM/dd/yyyy")),
            chartRequest.title))
      ).verifying("From date must be before To date", {
      chartRequest => chartRequest.dateFrom.isBefore(chartRequest.dateTo)
    })
  )


  def chart = Action {implicit request =>
    chartForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.index("The Wall Street Nerd", formWithErrors)),
      chartRequest => {
        LOG.debug("Equation: {}", chartRequest.equation)
        try {
          val equationsCompiled = chartRequest.equation.map(equationService.buildEquation(_))
          val eqResult = equationService.workOut(equationsCompiled, chartRequest.dateFrom, chartRequest.dateTo,chartRequest.scale)
          Ok(views.html.chart("The Wall Street Nerd %s".format(chartRequest.title),
            chartForm.fill(chartRequest), eqResult, chartRequest, eqResult.messages))
        }
        catch {
          case e: IllegalArgumentException =>
            LOG.error("Error: {}", e.getMessage)
            BadRequest(views.html.index("The Wall Street Nerd", chartForm.fill(chartRequest), errorMessages = Seq[String](e.getMessage)))
        }
      }
    )
  }

  def autoCompleteExpression(term: String) = Action {request =>
    val matches = autoComplete.findMatching(term).map(x => Map("id" -> x.name, "label" -> (x.name + ": " + x.desc), "value" -> x.name))
    Ok(toJson(matches))
  }

  def autoCompleteExpressionWtType(term: String) = Action {request =>
    val matches = autoComplete.findMatchingWtType(term).map {x =>
      val id = x._1.name + "." + x._2.name
      val label = id + ": " + x._1.desc + " " + x._2.desc
      Map("id" -> id, "label" -> label, "value" -> id)
    }
    Ok(toJson(matches))
  }

}
