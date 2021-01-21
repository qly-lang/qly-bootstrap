import java.nio.file.Path
import scala.util.parsing.input.Positional

class AST(val mexps: List[MExp]) {
  override def toString: String = mexps.mkString("(", " ", ")")
}

trait MExp extends Positional

class DotExp(val value: MExp, val dot: MExp) extends MExp {
  override def toString: String = "(. " + value.toString + " " + dot.toString + ")"
}
class ColonExp(val value: MExp, val col: MExp) extends MExp {
  override def toString: String = "(: " + value.toString + " " + col.toString + ")"
}
class CallExp(val value: MExp, val args: List[MExp]) extends MExp {
  override def toString: String = args.prepended(value).mkString("(", " ", ")")
}
class QuoteExp(val value: MExp) extends MExp {
  override def toString: String = "(' " + value.toString + ")"
}
class UnquoteExp(val value: MExp) extends MExp {
  override def toString: String = "(, " + value.toString + ")"
}
class SpliceExp(val value: MExp) extends MExp {
  override def toString: String = "(@ " + value.toString + ")"
}
class QlyArray(val value: List[MExp]) extends MExp {
  override def toString: String = if(value.isEmpty) "(array)" else value.mkString("(array ", " ", ")")
}
trait Atom extends MExp

class QlyString(val value: String) extends Atom {
  def unescapeString(v: String):String = {
    v.map(c => if (c=='"') {
      "\\\""
    } else if (c=='\\') {
      "\\\\"
    } else {
      c.toString
    }).mkString
  }
  override def toString: String = "\"" + unescapeString(value) + "\""
}
trait QlyReal extends Atom
trait QlyInt extends Atom
trait QlyUInt extends Atom {
  val base: Int
  def basePrefix: String = {
    if (base == 2) {
      "0b"
    } else if (base == 8) {
      "0o"
    } else{
      assert(base == 16)
      "0x"
    }
  }
}
class QlySymbol(val value: String) extends Atom {
  def unescapeSymbol(v: String): String = {
    val r = v.map(c => if (c=='|') {
      "\\|"
    } else if (c=='\\') {
      "\\\\"
    } else {
      c.toString
    }).mkString
    if("""[\s.\[\]:"\\',@#]""".r.findFirstIn(r).isDefined || """^\d+.*""".r.matches(r)) {
      '|' + r + '|'
    } else {
      r
    }
  }
  override def toString: String = unescapeSymbol(value)
}

class QlyInt8(val value: Byte) extends QlyInt {
  override def toString: String = value.toString
}
class QlyInt16(val value: Short) extends QlyInt {
  override def toString: String = value.toString
}
class QlyInt32(val value: Int) extends QlyInt {
  override def toString: String = value.toString
}
class QlyInt64(val value: Long) extends QlyInt {
  override def toString: String = value.toString
}
class QlyInt128(val value: BigInt) extends QlyInt {
  override def toString: String = value.toString
}
class QlyBigInt(val value: BigInt) extends QlyInt {
  override def toString: String = value.toString
}

class QlyFloat32(val value: Float) extends QlyReal {
  override def toString: String = value.toString
}
class QlyFloat64(val value: Double) extends QlyReal {
  override def toString: String = value.toString
}
class QlyDecimal(val value: BigDecimal) extends QlyReal {
  override def toString: String = value.toString
}

class QlyUInt8(val value: BigInt, val base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
class QlyUInt16(val value: BigInt, val base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
class QlyUInt32(val value: BigInt, val base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
class QlyUInt64(val value: BigInt, val base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
class QlyUInt128(val value: BigInt, val base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
class QlyBigUInt(val value: BigInt, val base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
