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
import org.apache.commons.codec.binary.Base64
import java.net.{URLEncoder, URL, URLConnection}
import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
import data.validation.Constraints._


object Application extends Controller {
  val LOG = LoggerFactory.getLogger(classOf[Application])

  def index = Action {
    Ok(views.html.index(chartForm.fill(ChartRequest(Seq(), Seq(), SingleScale, LocalDate.now().minusYears(2), LocalDate.now(), ""))))
  }

  def howItWorks = Action {
    Ok(views.html.howItWorks())
  }

  def about = Action {
    Ok(views.html.about())
  }

  def help = Action {
    Ok(views.html.help())
  }

  def contact = Action {
    Ok(views.html.contact())
  }

  val mailForm = Form(
    tuple(
      "name" -> text,
      "email" -> email.verifying(nonEmpty),
      "message" -> text
    ))

  def contactPost = Action {implicit request =>
    mailForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.contact(formError=Some("Please enter a valid email address")))
      },
      mailRequest => {
        var data = URLEncoder.encode("from", "UTF-8") + "=" + URLEncoder.encode("%s <%s>".format(mailRequest._1,mailRequest._2), "UTF-8")
        data += "&" + URLEncoder.encode("to", "UTF-8") + "=" + URLEncoder.encode("james.cutajar@gmail.com", "UTF-8")
        data += "&" + URLEncoder.encode("subject", "UTF-8") + "=" + URLEncoder.encode("post from geekyinvestor.com", "UTF-8")
        data += "&" + URLEncoder.encode("text", "UTF-8") + "=" + URLEncoder.encode(mailRequest._3, "UTF-8");
        LOG.info("Posting to mailgun: "+data)

        val url = new URL("https://api.mailgun.net/v2/app3995729.mailgun.org/messages")
        val uc = url.openConnection()
        val userPass = "api:%s".format(System.getProperty("MAILGUN_API_KEY"))
        val basicAuth = "Basic " + new String(new Base64().encode(userPass.getBytes))
        uc.setRequestProperty ("Authorization", basicAuth)
        uc.setDoOutput(true)
        val wr = new OutputStreamWriter(uc.getOutputStream)
        wr.write(data)
        wr.flush()
        val rd = new BufferedReader(new InputStreamReader(uc.getInputStream))
        var line:String=rd.readLine()
        while (line != null) {
          LOG.info("Received: {}",line)
          line = rd.readLine()
        }
        wr.close();
        rd.close();

        Ok(views.html.contact(message=Some("Your message has been successfully sent")))
      })
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
      formWithErrors => BadRequest(views.html.index(formWithErrors)),
      chartRequest => {
        LOG.debug("Equation: {}", chartRequest.equation)
        try {
          val equationsCompiled = chartRequest.equation.map(equationService.buildEquation(_))
          val eqResult = equationService.workOut(equationsCompiled, chartRequest.dateFrom, chartRequest.dateTo,chartRequest.scale)
          Ok(views.html.chart("Geeky Investor - %s".format(chartRequest.title),
            chartForm.fill(chartRequest), eqResult, chartRequest, eqResult.messages))
        }
        catch {
          case e: IllegalArgumentException =>
            LOG.error("Error: {}", e.getMessage)
            BadRequest(views.html.index(chartForm.fill(chartRequest), errorMessages = Seq[String](e.getMessage)))
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
