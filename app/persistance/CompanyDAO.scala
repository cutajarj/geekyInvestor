package persistance

import org.slf4j.LoggerFactory
import model.Company
import CompanyDAOImpl._
import com.mongodb.DBObject
import collection.JavaConversions._
import com.mongodb.BasicDBObject
import org.bson.types.ObjectId
import com.mongodb.DB
import org.joda.time.format.DateTimeFormat



trait CompanyDAO {
  def loadAllCompanies(): Seq[Company]
  def saveAllCompanies(companies: Seq[Company])
  def updateCompany(company: Company)
}

object CompanyDAOImpl {
  val LOG = LoggerFactory.getLogger(classOf[CompanyDAOImpl])
  val fmt = DateTimeFormat.forPattern("MM/dd/yyyy")
}



class CompanyDAOImpl(val mongoDB: DB) extends CompanyDAO {

  override def loadAllCompanies(): Seq[Company] = {
    LOG.debug("Loading all companies")
    val companyColl = mongoDB.getCollection("company")
    val results: Iterable[DBObject] = companyColl.find()
    val companies = results.map { entity =>
      Company(key = Some(entity.get("_id").toString),
        name = entity.get("n").toString,
        ric = entity.get("ric").toString,
        cik = Option(entity.get("cik")).map(_.toString).filter(!_.isEmpty),
        active = entity.get("a").toString,
        lastFundumentalUpdate = fmt.parseLocalDate(entity.get("lfe").toString))
    }
    companies.toSeq
  }

  override def saveAllCompanies(companies: Seq[Company]) {
    LOG.debug("Saving all companies")

    val companiesToInsert = companies.map { company =>
      val entity = new BasicDBObject()
      entity.put("n", company.name)
      entity.put("ric", company.ric)
      company.cik.map(entity.put("cik", _))
      entity.put("a", company.active)
      entity.put("lfe", company.lastFundumentalUpdate.toString(fmt))
      entity.asInstanceOf[DBObject]
    }.toList

    val stockTickerColl = mongoDB.getCollection("company")
    stockTickerColl.insert(companiesToInsert)
  }

  override def updateCompany(company: Company) = company.key.map { k =>
    LOG.debug("Updating {}", company)
    val stockTickerColl = mongoDB.getCollection("company")
    val updateCriteria = new BasicDBObject()
    updateCriteria.append("_id", new ObjectId(k))

    val entity = new BasicDBObject()
    entity.put("n", company.name)
    entity.put("ric", company.ric)
    company.cik.map(entity.put("cik", _))
    entity.put("a", company.active)
    entity.put("lfe", company.lastFundumentalUpdate.toString(fmt))
    entity.put("_id", new ObjectId(k))

    stockTickerColl.update(updateCriteria, entity)
  }

}