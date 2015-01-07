package model

import org.joda.time.LocalDate

case class Fundamental(companyKey: String, name: String, value: Double, fromDateOpt:Option[LocalDate], toDate:LocalDate, updatedDate:LocalDate)