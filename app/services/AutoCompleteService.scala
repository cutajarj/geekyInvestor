package services

import org.slf4j.LoggerFactory
import collection.mutable.{MapLike, Map}
import collection.immutable.TreeMap
import persistance.SymbolDAO
import AutoCompleteServiceImpl._
import model.{SymbolType, Symbol}

/**
 * Created with IntelliJ IDEA.
 * User: James
 * Date: 28/04/12
 * Time: 09:50
 * To change this template use File | Settings | File Templates.
 */

object AutoCompleteServiceImpl {
  val LOG = LoggerFactory.getLogger(classOf[AutoCompleteServiceImpl])
}

trait AutoCompleteService {
  def findMatching(prefix: String): Seq[Symbol]
  def findMatchingWtType(prefix:String): Seq[(Symbol,SymbolType)]
}

class AutoCompleteServiceImpl(symbolDAO: SymbolDAO) extends AutoCompleteService {
  val pTree = new PrefixMap[Symbol]
  val pTreeTypes = new PrefixMap[(Symbol,SymbolType)]

  def buildTree() {
    LOG.info("Building symbols Tree")
    val allSymbols = symbolDAO.loadAllSymbols()
    val allCurrencies  = symbolDAO.loadAllCurrencies()
    val allIndexes  = symbolDAO.loadAllIndexes()
    val tradedTypes = symbolDAO.loadAllTradedTypes()
    val allTypes = symbolDAO.loadAllFundamentalTypes() ++ tradedTypes
    allSymbols.foreach {s =>
      pTree.update(s.name.toUpperCase,s)
      pTree.update(s.desc.toUpperCase,s)
      allTypes.foreach {t=>
        pTreeTypes.update(s.name+"."+t.name,(s,t))
        pTreeTypes.update(s.name+"."+t.desc,(s,t))
      }
    }
    allCurrencies.foreach { c1=>
      allCurrencies.foreach { c2=>
         if (c1!=c2) pTree.update(c1.name+c2.name, Symbol(c1.name+c2.name,c1.desc+" to "+c2.desc,"",""))
      }
    }
    allIndexes.foreach{ i=>
      pTree.update(i.name.toUpperCase,i)
      pTree.update(i.desc.toUpperCase,i)
      tradedTypes.foreach{t=>
        pTreeTypes.update(i.name+"."+t.name,(i,t))
        pTreeTypes.update(i.name+"."+t.desc,(i,t))
      }
    }
    LOG.info("Symbols Tree built, with {} entries",pTree.size)
    LOG.info("Symbols Tree Type built, with {} entries",pTreeTypes.size)
  }

  override def findMatching(str: String): Seq[Symbol] = {
    LOG.debug("Finding matches for prefix {}",str)
    val matches = pTree.withPrefix(str.toUpperCase).slice(0,20).toSet.slice(0,7).toSeq
    LOG.debug("Found matches {}",matches)
    matches
  }

  override def findMatchingWtType(str: String): Seq[(Symbol,SymbolType)] = {
    LOG.debug("Finding matches wt type for prefix {}",str)
    val matches = pTreeTypes.withPrefix(str.toUpperCase).slice(0,20).toSet.slice(0,7).toSeq
    LOG.debug("Found matches {}",matches)
    matches
  }

}

sealed class PrefixMap[T] extends Map[String, T] with MapLike[String, T, PrefixMap[T]] {
  var suffixes: TreeMap[Char, PrefixMap[T]] = TreeMap()
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  def withPrefix(str: String): Iterator[T] = str match {
    case "" => this.iterator.map(_._2)
    case s =>
      suffixes get s(0) match {
        case None =>
          Iterator[T]()
        case Some(x) =>
          x.withPrefix(s substring 1)
      }
  }

  def withPrefixAddEmpty(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefixAddEmpty (s substring 1)
    }

  override def update(s: String, elem: T) =
    withPrefixAddEmpty(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) {
      val prev = value;
      value = None;
      prev
    }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def +=(kv: (String, T)): this.type = {
    update(kv._1, kv._2);
    this
  }

  def -=(s: String): this.type = {
    remove(s);
    this
  }

  override def empty = new PrefixMap[T]
}