package s2j

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import s2j.MyTransformer.WriteToMapOps.toJavaMap

import scala.jdk.CollectionConverters._

import java.util.{ Map => JMap }

class Test1 extends AnyFreeSpec with Matchers:
  case class Str(s: String)
  case class Bool(b: Boolean)
  case class ClassInt(i: Int)
  case class ClassLong(l: Long)
  case class AllPrimitives(
    c: Char,
    by: Byte,
    s: Short,
    i: Int,
    l: Long,
    f: Float,
    d: Double,
    b: Boolean
  )

  "Int field" in {
    val c = ClassInt(2)
    val jmap: JMap[String, AnyRef] = c.toJavaMap
    
    jmap.get("i") mustBe a[Integer]
  }

  "Long field" in {
    val c = ClassLong(2L)
    val jmap: JMap[String, AnyRef] = c.toJavaMap
    
    jmap.get("l") mustBe a[java.lang.Long]
  }

  "All primitive fields" in {
    val c = AllPrimitives('c', 1, 2, 3, 4, 1.1, 2.2, true)
    val jmap: JMap[String, AnyRef] = c.toJavaMap
    
    jmap.get("c")  mustBe a[java.lang.Character]
    jmap.get("by") mustBe a[java.lang.Byte]
    jmap.get("s")  mustBe a[java.lang.Short]
    jmap.get("i")  mustBe a[java.lang.Integer]
    jmap.get("l")  mustBe a[java.lang.Long]
    jmap.get("f")  mustBe a[java.lang.Float]
    jmap.get("d")  mustBe a[java.lang.Double]
    jmap.get("b")  mustBe a[java.lang.Boolean]
  }

  "More than one primitive" in {
    case class StrAndInt(sss: Str, iii: ClassInt)
    val c = StrAndInt(Str("a"), ClassInt(5))
    val jmap: JMap[String, AnyRef] = c.toJavaMap
    
    jmap mustEqual Map("sss" -> Map("s" -> "a").asJava, "iii" -> Map("i" -> 5).asJava).asJava
  }

  "with List" in {
    case class WithList(list: List[String])
    val c = WithList(List("a", "b", "c"))
    val jmap: JMap[String, AnyRef] = c.toJavaMap

    jmap mustEqual Map("list" -> List("a", "b", "c").asJava).asJava
  }

  "with another List" in {
    case class WithLists(l1: List[String], l2: List[Boolean])
    val c = WithLists(List("eita"), List(true, false))
    val jmap: JMap[String, AnyRef] = c.toJavaMap

    jmap mustEqual Map("l1" -> List("eita").asJava, "l2" -> List(true, false).asJava).asJava
  }

  "with Option some" in {
    case class Opt(o: Option[String])
    val c = Opt(Some("a"))
    val jmap: JMap[String, AnyRef] = c.toJavaMap

    jmap mustEqual Map("o" -> "a").asJava
  }

  "with Option none" in {
    case class Opt(o: Option[String])
    val c = Opt(None)
    val jmap: JMap[String, AnyRef] = c.toJavaMap

    jmap mustEqual Map("o" -> null).asJava
  }

