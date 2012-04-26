package persistance

import model.Stat
import java.util.Date
import com.mongodb.{DBObject, BasicDBObject, DB}

/*import com.google.appengine.api.datastore.Query.SortDirection
import com.google.appengine.api.datastore._*/

import org.slf4j.LoggerFactory
import collection.JavaConversions._


/**
 * Created by IntelliJ IDEA.
 * User: James Cutajar
 * Date: 12/02/12
 * Time: 10:07
 * To change this template use File | Settings | File Templates.
 */

trait StatDAO {
  def save(stat: Stat): Stat

  def saveAll(stats: Iterable[Stat])

  def deleteAll()

  def update(stat: Stat)

  def loadLatest(t: String, s: String): Stat

  def load(t: Option[String] = None, s: Option[String] = None): Iterable[Stat]
}


object StatDAOImpl {
  val LOG = LoggerFactory.getLogger(classOf[StatDAOImpl])
}

import StatDAOImpl._

class StatDAOImpl(val mongoDB: DB) extends StatDAO {

  def save(stat: Stat): Stat = {
    LOG.debug("Saving {}", stat)
    val fundamentalColl = mongoDB.getCollection("fundamentalColl")
    val entity = new BasicDBObject()

    entity.put("t", stat.statType)
    entity.put("s", stat.symbol)
    entity.put("v", stat.value)
    entity.put("ts", stat.timeStamp.getTime)

    fundamentalColl.insert(entity)

    stat.copy(key = Some(entity.get("_id").toString))
  }

  def saveAll(stats: Iterable[Stat]) {
    LOG.debug("Saving all")
    val fundamentalColl = mongoDB.getCollection("fundamentalColl")
    val entities = stats.map {stat =>
      val entity = new BasicDBObject()
      entity.put("t", stat.statType)
      entity.put("s", stat.symbol)
      entity.put("v", stat.value)
      entity.put("ts", stat.timeStamp.getTime)

      entity.asInstanceOf[DBObject]
    }.toList

    entities.grouped(5000).foreach {listOfEntities =>
      fundamentalColl.insert(listOfEntities)
    }

  }


  def update(stat: Stat) = stat.key.map {
    k =>
      LOG.debug("Updating {}", stat)
      /*val datastore = DatastoreServiceFactory.getDatastoreService
      val entity = new Entity(k)

      entity.setProperty("statType", stat.statType)
      entity.setProperty("symbol", stat.symbol)
      entity.setProperty("value", stat.value)
      entity.setProperty("timeStamp", stat.timeStamp)
      entity.setProperty("value", stat.value)

      datastore.put(entity)*/
      null
  }


  def loadLatest(t: String, s: String): Stat = {
    //LOG.debug("Loading Latest {}.{}",s,t)
    /*val dataStore = DatastoreServiceFactory.getDatastoreService
    val query = new Query("Stat")
    query.addFilter("statType", Query.FilterOperator.EQUAL, t)
    query.addFilter("symbol", Query.FilterOperator.EQUAL, s)
    query.addSort("timeStamp", SortDirection.DESCENDING)
    val pq = dataStore.prepare(query)

    val stats = asStats(pq.asIterable(FetchOptions.Builder.withLimit(1)))
    if (stats.nonEmpty)
      stats.head
    else
      throw new IllegalArgumentException("Unable to find symbol: %s.%s".format(s, t))
      */
    null
  }

  def load(t: Option[String] = None, s: Option[String] = None): Iterable[Stat] = {
    LOG.debug("Loading {}.{}", s, t)
    val fundamentalColl = mongoDB.getCollection("fundamentalColl")
    val search = new BasicDBObject()
    t.map(search.put("t", _))
    s.map(search.put("s", _))

    val result = fundamentalColl.find(search)

    val stats = asStats(result)

    if (stats.nonEmpty)
      stats
    else
      throw new IllegalArgumentException("Unable to find symbol: %s.%s".format(s.getOrElse(""), t.getOrElse("")))
  }

  def deleteAll() {
    LOG.debug("Deleting all")
    val fundamentalColl = mongoDB.getCollection("fundamentalColl")
    fundamentalColl.remove(new BasicDBObject())
  }

  private[this] def asStats(e: Iterable[DBObject]) = e.map {
    entity =>
      new Stat(Some(entity.get("_id").toString),
        entity.get("v").asInstanceOf[Double],
        new Date(entity.get("ts").asInstanceOf[Long]),
        entity.get("t").toString,
        entity.get("s").toString)
  }

}

