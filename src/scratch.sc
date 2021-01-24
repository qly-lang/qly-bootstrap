
import scala.collection.mutable.Map
val c:Map[String, Int] = Map("a" -> 3)
c("a")
c.getOrElse("b", None)