package controllers

/**
 * Created with IntelliJ IDEA.
 * User: James Cutajar
 * Date: 14/04/12
 * Time: 16:18
 * To change this template use File | Settings | File Templates.
 */

import play.api._
import data.Form
import data.Forms._
import play.api.mvc._
import org.slf4j.LoggerFactory
import java.text.SimpleDateFormat
import model.Stat
import config.AppConfig._

object Admin extends Controller {
  val LOG = LoggerFactory.getLogger(classOf[Controller])

  val uploadForm = Form(
    ("fundamental" -> text)
  )

  def index = Action {
    Ok(views.html.admin.index("The Wall Street Nerd"))
  }

  def updateFundamentals = Action {
    Ok(views.html.admin.index("The Wall Street Nerd"))
  }

  def uploadFundamentals = Action {
    Ok(views.html.admin.uploadFundamentals())
  }

  def clearAllFundamentals = Action {
    Ok(views.html.admin.clearAllFundamentals())
  }

  def postClearAllFundamentals = Action {
    LOG.info("Clearing all fundamentals!")
    fundamentalDAO.deleteAll()
    Ok(views.html.admin.index("The Wall Street Nerd"))
  }

  def postFundamentals = Action(parse.multipartFormData) {request =>
    request.body.file("fundamentals").map {fundList =>
      val bufferedSource = io.Source.fromFile(fundList.ref.file)
      val sdf = new SimpleDateFormat("MM-dd-yyyy")
      val stats = bufferedSource.getLines().map {line =>
        line.split(",").toList match {
          case s :: t :: v :: d :: Nil => Stat(symbol = s, value = v.toDouble, timeStamp = sdf.parse(d), statType = t)
          case _ =>
            LOG.error("fail parsing...")
            throw new IllegalStateException("Failed to parse")
        }
      }
      fundamentalDAO.saveAll(stats.toIterable)

      Ok(views.html.admin.index("The Wall Street Nerd"))
    }.getOrElse {
      BadRequest(views.html.admin.uploadFundamentals())
    }
  }
}
