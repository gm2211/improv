package storage

import java.io.File

import cbr.MapStore
import com.google.common.io.Files
import org.mapdb.{DB, DBMaker, HTreeMap}

import scala.jdk.CollectionConverters._

object MapDBMapStore {
  def loadFromFile[K, V](filename: String) = {
    val db = DBMaker.fileDB(new File(filename)).make()
    new MapDBMapStore[K, V](db, Files.getNameWithoutExtension(filename))
  }
}

class MapDBMapStore[K, V](private val db: DB, mapName: String) extends MapStore[K, V] {

  private val db_map: HTreeMap[K, V] = db.hashMap(mapName)

  override def get(key: K): Option[V] = Option(db_map.get(key))

  override def remove(key: K): Unit = db_map.remove(key)

  override def put(key: K, value: V): Unit = db_map.put(key, value)

  override def removeAll(): Unit = db_map.keySet().asScala.toList.foreach(k => db_map.remove(k))

  def contains(key: K): Boolean = db_map.containsKey(key)

  def keySet(): Set[K] = db_map.keySet().asScala.toSet

  def close(): Unit = db.close()

  /**
   * Commits the changes to the map
   */
  override def commit(): Unit = db.commit()
}
