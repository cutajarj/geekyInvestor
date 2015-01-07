package model

import org.joda.time.LocalDate

case class Company(key: Option[String] = None, name: String, ric: String, cik: Option[String], lastFundumentalUpdate:LocalDate, active: String = "A")
