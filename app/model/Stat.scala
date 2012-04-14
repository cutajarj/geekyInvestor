package model

import java.util.Date

/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 11/02/12
 * Time: 18:40
 * To change this template use File | Settings | File Templates.
 */

case class Stat(key: Option[String] = None, value: Double, timeStamp: Date, statType: String, symbol: String) {

}
