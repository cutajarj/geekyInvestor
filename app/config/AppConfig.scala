package config

import persistance.{SymbolDAOImpl, SymbolDAO, StatDAO, StatDAOImpl}
import com.mongodb.{BasicDBObject, MongoURI}
import parser.ExprParser
import services._


/**
 * Created with IntelliJ IDEA.
 * User: James Cutajar
 * Date: 14/04/12
 * Time: 22:08
 * To change this template use File | Settings | File Templates.
 */

object AppConfig {
  lazy val fundamentalDAO:StatDAO = new StatDAOImpl(mongoDB)
  lazy val symbolDAO:SymbolDAO = new SymbolDAOImpl

  lazy val statService:StatService = new StatServiceImpl(fundamentalDAO)
  lazy val exprParser:ExprParser = new ExprParser()
  lazy val equationService:EquationService = new EquationServiceImpl(exprParser,statService)

  lazy val autoComplete:AutoCompleteService = {
    val autoCompleteService = new AutoCompleteServiceImpl(symbolDAO)
    autoCompleteService.buildTree()
    autoCompleteService
  }

  lazy val mongoDB = {
    val mongoURI = new MongoURI(System.getProperty("MONGOHQ_URL"))
    val db = mongoURI.connectDB()
    Option(mongoURI.getUsername).foreach(username=>db.authenticate(username, mongoURI.getPassword))
    val index = new BasicDBObject()
    index.put("t", 1)
    index.put("s", 1)

    db.getCollection("fundamentalColl").ensureIndex(index)
    db
  }
}
