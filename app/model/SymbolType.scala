package model

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 03/04/12
 * Time: 19:41
 * To change this template use File | Settings | File Templates.
 */

case class SymbolType(name: String, desc: String, remoteId: Option[String], appliesTo: String, remoteConversion: String => Option[Double]) {

}
