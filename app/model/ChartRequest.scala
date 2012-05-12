package model

import org.joda.time.LocalDate

/**
 * Created with IntelliJ IDEA.
 * User: James
 * Date: 07/05/12
 * Time: 09:15
 * To change this template use File | Settings | File Templates.
 */

case class ChartRequest(equation: Seq[String], yColorValue: Seq[String], scale: ScaleType, dateFrom: LocalDate, dateTo: LocalDate, title: String)
