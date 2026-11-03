package utils

import java.io.ByteArrayInputStream

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility
import com.fasterxml.jackson.annotation.PropertyAccessor
import tools.jackson.databind.json.JsonMapper
import tools.jackson.databind.DeserializationFeature
import tools.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}

import scala.reflect.ClassTag
import scala.util.Try

object SerialisationUtils {
  val mapper: JsonMapper & ClassTagExtensions =
    JsonMapper.builder()
      .addModule(DefaultScalaModule)
      .changeDefaultVisibility(_.withVisibility(PropertyAccessor.ALL, Visibility.ANY))
      .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
      .build() :: ClassTagExtensions

  def serialise[A](obj: A): Try[Array[Byte]] = Try {
    mapper.writeValueAsBytes(obj)
  }

  def serialise[A](obj: A, filename: String): Try[Boolean] =
    serialise(obj).flatMap(IOUtils.write(filename, _))

  def deserialise[A: ClassTag](bytes: Array[Byte]): Try[A] = {
    val stream = new ByteArrayInputStream(bytes)
    // Deliberately the erased class: a ClassTag has no type arguments, so constructing a JavaType
    // for a generic class (e.g. KDTreeIndex[T]) would fail. Polymorphic content carries "@class".
    Try {
      mapper.readValue(stream, implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]])
    }
  }

  def deserialise[A: ClassTag](bytes: Array[Byte], a: Class[A]): Try[A] = {
    val stream = new ByteArrayInputStream(bytes)
    Try {
      mapper.readValue(stream, a)
    }
  }

  def deserialise[A: ClassTag](filename: String, a: Class[A]): Try[A] =
    IOUtils.read(filename).flatMap { case bytes => deserialise(bytes, a) }

  def deserialise[A: ClassTag](filename: String): Try[A] =
    IOUtils.read(filename).flatMap { case bytes => deserialise(bytes) }
}
