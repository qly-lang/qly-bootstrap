import scala.util.parsing.input.{Position, Positional}

class AST(val mexps: List[MExp]) {
  override def toString: String = mexps.mkString("(", " ", ")")
}

sealed trait MExp extends Positional

case class ASTPosition(position: Position) extends Positional {
  pos = position
}

case class DotExp(value: MExp, dot: MExp) extends MExp {
  override def toString: String = "(. " + value.toString + " " + dot.toString + ")"
}
case class ColonExp(value: MExp, col: MExp) extends MExp {
  override def toString: String = "(: " + value.toString + " " + col.toString + ")"
}
case class CallExp(value: MExp, args: List[MExp]) extends MExp {
  override def toString: String = args.prepended(value).mkString("(", " ", ")")
}
case class QuoteExp(value: MExp) extends MExp {
  override def toString: String = "(' " + value.toString + ")"
}
case class UnquoteExp(value: MExp) extends MExp {
  override def toString: String = "(, " + value.toString + ")"
}
case class SpliceExp(value: MExp) extends MExp {
  override def toString: String = "(@ " + value.toString + ")"
}
case class QlyArray(value: List[MExp]) extends MExp {
  override def toString: String =
    if (value.isEmpty) "(array)" else value.mkString("(array ", " ", ")")
}
sealed trait Atom extends MExp

case class QlyString(value: String) extends Atom {
  def unescapeString(v: String): String = {
    v.map(c =>
        if (c == '"') {
          "\\\""
        } else if (c == '\\') {
          "\\\\"
        } else {
          c.toString
        }
      )
      .mkString
  }
  override def toString: String = "\"" + unescapeString(value) + "\""
}
sealed trait QlyReal extends Atom
sealed trait QlyInt extends Atom
sealed trait QlyUInt extends Atom {
  val base: Int
  def basePrefix: String = {
    if (base == 2) {
      "0b"
    } else if (base == 8) {
      "0o"
    } else {
      assert(base == 16)
      "0x"
    }
  }
}

case class QlySymbol(value: String) extends Atom {
  def unescapeSymbol(v: String): String = {
    val r = v
      .map(c =>
        if (c == '|') {
          "\\|"
        } else if (c == '\\') {
          "\\\\"
        } else {
          c.toString
        }
      )
      .mkString
    if ("""[\s.\[\]:"\\',@#]""".r.findFirstIn(r).isDefined || """^\d+.*""".r.matches(r)) {
      '|' + r + '|'
    } else {
      r
    }
  }
  override def toString: String = unescapeSymbol(value)
}

case class QlyInt8(value: Byte) extends QlyInt {
  override def toString: String = value.toString
}
case class QlyInt16(value: Short) extends QlyInt {
  override def toString: String = value.toString
}
case class QlyInt32(value: Int) extends QlyInt {
  override def toString: String = value.toString
}
case class QlyInt64(value: Long) extends QlyInt {
  override def toString: String = value.toString
}
case class QlyInt128(value: BigInt) extends QlyInt {
  override def toString: String = value.toString
}
case class QlyBigInt(value: BigInt) extends QlyInt {
  override def toString: String = value.toString
}

case class QlyFloat32(value: Float) extends QlyReal {
  override def toString: String = value.toString
}
case class QlyFloat64(value: Double) extends QlyReal {
  override def toString: String = value.toString
}
case class QlyDecimal(value: BigDecimal) extends QlyReal {
  override def toString: String = value.toString
}

case class QlyUInt8(value: BigInt, base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
case class QlyUInt16(value: BigInt, base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
case class QlyUInt32(value: BigInt, base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
case class QlyUInt64(value: BigInt, base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
case class QlyUInt128(value: BigInt, base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
case class QlyBigUInt(value: BigInt, base: Int) extends QlyUInt {
  override def toString: String = this.basePrefix + value.toString(base)
}
