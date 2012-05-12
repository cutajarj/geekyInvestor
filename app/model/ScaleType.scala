package model

/**
 * Created with IntelliJ IDEA.
 * User: James
 * Date: 06/05/12
 * Time: 17:06
 * To change this template use File | Settings | File Templates.
 */

object ScaleType {
  def fromName(str: String): ScaleType = str match {
    case "SingleScale" => SingleScale
    case "MultiScale" => MultiScale
    case "RebaseScale" => RebaseScale
  }
}

sealed trait ScaleType {
  def st: String
}

case object SingleScale extends ScaleType {
  val st = "SingleScale"
}

case object MultiScale extends ScaleType {
  val st = "MultiScale"
}

case object RebaseScale extends ScaleType {
  val st = "RebaseScale"
}