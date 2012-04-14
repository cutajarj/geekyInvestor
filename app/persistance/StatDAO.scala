package persistance

import model.Stat
import java.util.Date
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

  def update(stat: Stat)

  def loadLatest(t: String, s: String): Stat

  def load(t: Option[String] = None, s: Option[String] = None): Iterable[Stat]
}


object StatDAOImpl {
  val LOG = LoggerFactory.getLogger(classOf[StatDAOImpl])
}

import StatDAOImpl._

class StatDAOImpl extends StatDAO {

  def save(stat: Stat): Stat = {
    LOG.debug("Saving {}", stat)
    /*val datastore = DatastoreServiceFactory.getDatastoreService
    val entity = new Entity("Stat")

    entity.setProperty("statType", stat.statType)
    entity.setProperty("symbol", stat.symbol)
    entity.setProperty("value", stat.value)
    entity.setProperty("timeStamp", stat.timeStamp)
    entity.setProperty("value", stat.value)

    datastore.put(entity)

    stat.copy(key = Some(entity.getKey))*/
    null
  }

  def saveAll(stats: Iterable[Stat]) {
    LOG.debug("Saving all")
    /*val datastore = DatastoreServiceFactory.getDatastoreService
    val entities: java.lang.Iterable[Entity] = asJavaIterable(stats.map {
      stat =>
        val entity = new Entity("Stat")
        entity.setProperty("statType", stat.statType)
        entity.setProperty("symbol", stat.symbol)
        entity.setProperty("value", stat.value)
        entity.setProperty("timeStamp", stat.timeStamp)
        entity.setProperty("value", stat.value)
        entity
    })
    datastore.put(entities) */
    null
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
    /*val dataStore = DatastoreServiceFactory.getDatastoreService
    val query = new Query("Stat")
    t.map(query.addFilter("statType", Query.FilterOperator.EQUAL, _))
    s.map(query.addFilter("symbol", Query.FilterOperator.EQUAL, _))
    query.addSort("timeStamp")
    val pq = dataStore.prepare(query)

    val stats = asStats(pq.asIterable())

    if (stats.nonEmpty)
      stats
    else
      throw new IllegalArgumentException("Unable to find symbol: %s.%s".format(s.getOrElse(""), t.getOrElse("")))
      */
    null
  }

/*  private[this] def asStats(e: Iterable[Entity]) = e.map {
    entity =>
      new Stat(Some(entity.getKey),
        entity.getProperty("value").asInstanceOf[Double],
        entity.getProperty("timeStamp").asInstanceOf[Date],
        entity.getProperty("statType").toString,
        entity.getProperty("symbol").toString)
  }*/

}

