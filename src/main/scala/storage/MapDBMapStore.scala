package storage

import java.io.File

import cbr.MapStore
import com.google.common.io.Files
import org.mapdb.{DB, DBMaker, HTreeMap, Serializer}

import scala.jdk.CollectionConverters._

object MapDBMapStore {
  // MapDB 3 locks a store file exclusively, but every musician deserialises its own index and so opens
  // the same file: share one DB instance per file within the JVM (reference counted, closed with the last user).
  private val openDBs = new java.util.HashMap[String, (DB, Int)]()

  def loadFromFile[K, V](filename: String) = {
    val file = new File(filename).getCanonicalFile
    val key = file.getPath
    val db = openDBs.synchronized {
      val shared = Option(openDBs.get(key)).collect { case (db, users) if !db.isClosed => (db, users) }
      val db = shared.map(_._1).getOrElse {
        // MapDB 3 rejects an empty file ("File is empty"), but an empty placeholder is what
        // KDTreeIndex.removeDefault leaves behind (and what is checked in): start from scratch instead.
        if (file.exists() && file.length() == 0) file.delete()
        DBMaker.fileDB(file).transactionEnable().make()
      }
      openDBs.put(key, (db, shared.map(_._2).getOrElse(0) + 1))
      db
    }
    new MapDBMapStore[K, V](db, Files.getNameWithoutExtension(filename), key)
  }

  private def release(key: String): Unit = openDBs.synchronized {
    Option(openDBs.get(key)).foreach { case (db, users) =>
      if (users <= 1) {
        openDBs.remove(key)
        if (!db.isClosed) db.close()
      } else openDBs.put(key, (db, users - 1))
    }
  }
}

class MapDBMapStore[K, V](private val db: DB, mapName: String, private val registryKey: String = null) extends MapStore[K, V] {

  private val db_map: HTreeMap[K, V] = db.hashMap(mapName, Serializer.STRING, Serializer.JAVA).createOrOpen().asInstanceOf[HTreeMap[K, V]]

  override def get(key: K): Option[V] = Option(db_map.get(key))

  override def remove(key: K): Unit = db_map.remove(key)

  override def put(key: K, value: V): Unit = db_map.put(key, value)

  override def removeAll(): Unit = db_map.keySet().asScala.toList.foreach(k => db_map.remove(k))

  def contains(key: K): Boolean = db_map.containsKey(key)

  def keySet(): Set[K] = db_map.keySet().asScala.toSet

  def close(): Unit = if (registryKey == null) db.close() else MapDBMapStore.release(registryKey)

  /**
   * Commits the changes to the map
   */
  override def commit(): Unit = db.commit()
}
