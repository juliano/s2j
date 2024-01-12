package s2j

import java.util.{ Map => JMap, List => JList }
import scala.compiletime.{ constValue, erasedValue, summonFrom, summonInline }
import scala.deriving._
import scala.jdk.CollectionConverters._
import scala.quoted._

object MyTransformer {

  enum JavaOutput:
    def value: AnyRef
    case Leaf(value: AnyRef) extends JavaOutput
    case Node(value: JMap[String, AnyRef]) extends JavaOutput

  import JavaOutput._

  inline given toJavaDerived[T]: ToJava[T] = ToJava.derived

  object WriteToMapOps {
    extension [T](value: T)(using tj: ToJava[T])
      def toJavaMap: JMap[String, AnyRef] =
        tj.writeToMap(value) match
          case Node(map) => map
          case _ => throw new IllegalArgumentException("Not a product type")
  }

  trait FromJava[T]:
    def from(jmap: JMap[String, AnyRef]): T

  trait ToJava[T]:
    def writeToMap(value: T): JavaOutput

  object ToJava {
    inline def summonInstance[T]: ToJava[T] = summonInline[ToJava[T]]

    given writeString: ToJava[String] with
      def writeToMap(value: String) = JavaOutput.Leaf(value)

    given toChar: ToJava[Char] with
      def writeToMap(value: Char) = JavaOutput.Leaf(Char.box(value))

    given toByte: ToJava[Byte] with
      def writeToMap(value: Byte) = JavaOutput.Leaf(Byte.box(value))

    given toShort: ToJava[Short] with
      def writeToMap(value: Short) = JavaOutput.Leaf(Short.box(value))

    given toInteger: ToJava[Int] with
      def writeToMap(value: Int) = JavaOutput.Leaf(Int.box(value))

    given toLong: ToJava[Long] with
      def writeToMap(value: Long) = JavaOutput.Leaf(Long.box(value))

    given toFloat: ToJava[Float] with
      def writeToMap(value: Float) = JavaOutput.Leaf(Float.box(value))

    given toDouble: ToJava[Double] with
      def writeToMap(value: Double) = JavaOutput.Leaf(Double.box(value))

    given toBoolean: ToJava[Boolean] with
      def writeToMap(value: Boolean) = JavaOutput.Leaf(Boolean.box(value))

    given aList[T](using tj: ToJava[T]): ToJava[List[T]] with
      def writeToMap(list: List[T]) = JavaOutput.Leaf(list.map(tj.writeToMap(_).value).asJava)

    given anOption[T](using tj: ToJava[T]): ToJava[Option[T]] with
      def writeToMap(opt: Option[T]) = JavaOutput.Leaf(opt.map(tj.writeToMap(_).value).orNull)

    inline def recurseSum[Types <: Tuple, T](element: T): JavaOutput =
      inline erasedValue[Types] match
        case _: (tpe *: types) =>
          if (element.isInstanceOf[tpe])
            summonInstance[tpe].writeToMap(element.asInstanceOf[tpe])
          else
            recurseSum[types, T](element)
        case _: EmptyTuple =>
          throw new IllegalArgumentException(s"Invalid coproduct type")

    inline def recurse[Names <: Tuple, Types <: Tuple](element: Product)(index: Int): Map[String, AnyRef] =
      inline erasedValue[(Names, Types)] match
        case (_: (name *: names), _: (tpe *: types)) =>
          val key = constValue[name].toString
          val value = element.productElement(index).asInstanceOf[tpe]
          val writtenValue = summonInstance[tpe].writeToMap(value).value
          recurse[names, types](element)(index + 1) + (key -> writtenValue)
        case (_: EmptyTuple, _) =>
          Map.empty[String, AnyRef]

    inline def derived[T]: ToJava[T] =
      inline summonInline[Mirror.Of[T]] match
        case prod: Mirror.ProductOf[T] =>
          new ToJava[T]:
            def writeToMap(value: T): JavaOutput =
              Node(recurse[prod.MirroredElemLabels, prod.MirroredElemTypes](value.asInstanceOf[Product])(0).asJava)
        case sum: Mirror.SumOf[T] =>
          new ToJava[T]:
            def writeToMap(value: T): JavaOutput = recurseSum[sum.MirroredElemTypes, T](value)
        case _ =>
          throw new IllegalArgumentException(s"Mirror not found")
  }
}
