trait Value

case object NilValue extends Value
case class SymbolValue(value: String) extends Value
case class Int8Value(value: Byte) extends Value
case class Int16Value(value: Short) extends Value
case class Int32Value(value: Int) extends Value
case class Int64Value(value: Long) extends Value
case class Int128Value(value: BigInt) extends Value
case class BigIntValue(value: BigInt) extends Value
case class UInt8Value(value: BigInt, base: Int) extends Value
case class UInt16Value(value: BigInt, base: Int) extends Value
case class UInt32Value(value: BigInt, base: Int) extends Value
case class UInt64Value(value: BigInt, base: Int) extends Value
case class UInt128Value(value: BigInt, base: Int) extends Value
case class BigUIntValue(value: BigInt, base: Int) extends Value
case class Float32Value(value: Float) extends Value
case class Float64Value(value: Double) extends Value
case class DecimalValue(value: BigDecimal) extends Value
case class BoolValue(value: Boolean) extends Value

case class ArrayValue(value: IndexedSeq[Value]) extends Value
case class FunValue() extends Value
case class StructValue() extends Value
