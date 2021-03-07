import scala.collection.mutable
import scala.util.parsing.input.Positional

case class SemPos(tree: SemTree, i: Int)

class SemTree(val exps: IndexedSeq[SemExp], val scope: Scope) {
  scope.pendingGotos.foreach(goto =>
    goto.goto.append(SemPos(this, exps.indexOf(scope.tags(goto.pos.asInstanceOf[CallExp].args.head.asInstanceOf[QlySymbol].value).get)))
  )
}

trait SemExp {
  val t: TypeExp
  val pos: Positional

  def s: S
}

case class SemErrorExp(error: SemanticError, pos: MExp) extends SemExp {
  override val t = ErrorType
  override def s = S("Error")
}

trait Literal extends SemExp {
  val pos: Positional
}
case class NilLiteral(pos: Positional) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
  override def s = S("Nil")
}
case class Int32Literal(pos: QlyInt32) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("int32").get)
  override def s = S("Int32", S(pos.value.toString))
}
case class Int64Literal(pos: QlyInt64) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("int64").get)
  override def s = S("Int64", S(pos.value.toString))
}
case class Int128Literal(pos: QlyInt128) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("int128").get)
  override def s = S("Int128", S(pos.value.toString))
}
case class BigIntLiteral(pos: QlyBigInt) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("bigint").get)
  override def s = S("BigInt", S(pos.value.toString))
}
case class UInt32Literal(pos: QlyUInt32) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("uint32").get)
  override def s = S("UInt32", S(pos.toString))
}
case class UInt64Literal(pos: QlyUInt64) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("uint64").get)
  override def s = S("UInt64", S(pos.toString))
}
case class UInt128Literal(pos: QlyUInt128) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("uint128").get)
  override def s = S("UInt128", S(pos.toString))
}
case class BigUIntLiteral(pos: QlyBigUInt) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("biguint").get)
  override def s = S("BigUInt", S(pos.toString))
}
case class Float64Literal(pos: QlyFloat64) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("float64").get)
  override def s = S("Float64", S(pos.value.toString))
}
case class DecimalLiteral(pos: QlyDecimal) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("decimal").get)
  override def s = S("Decimal", S(pos.value.toString))
}
case class StringLiteral(pos: QlyString) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("string").get)
  override def s = S("String", S(pos.value))
}
case class BoolLiteral(pos: QlySymbol) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("bool").get)
  override def s = S("Bool", S(pos.value))
}
case class SymbolLiteral(pos: QlySymbol) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("symbol").get)
  override def s = S("Symbol", S(pos.value))
}
case class ArrayExp(pos: Positional, t: ArrayType, elems: Seq[SemExp]) extends SemExp {
  val a = S(t.elemType.toString) +: elems.map(s => s.s).toArray
  override def s = S("Array", a: _*)
}

case class VarRef(pos: QlySymbol, varDef: VarDef) extends SemExp {
  override val t: TypeExp = varDef.t
  override def s = S("VarRef", S(pos.value))
}
case class FunCall(pos: CallExp, f: SemExp, args: Seq[SemExp]) extends SemExp {
  override val t: TypeExp = f.t.asInstanceOf[FunType].returnType
  val a = f.s +: args.map(_.s).toArray
  override def s = S("FunCall", a: _*)
}
case class ArrayAccess(pos: CallExp, a: SemExp, idx: SemExp) extends SemExp {
  override val t: TypeExp = a.t.asInstanceOf[ArrayType].elemType
  override def s = S("ArrayAccess", a.s, idx.s)
}
case class StructFieldAccess(pos: MExp, a: SemExp, field: SymbolValue) extends SemExp {
  override val t: TypeExp = a.t.asInstanceOf[StructType].fields.find(fi => fi.name == field.value).get.t
  override def s = S("StructFieldAccess", a.s, S(field.value))
}
case class IfOp(pos: MExp, condition: SemExp, thenClause: SemExp, elseClause: Option[SemExp]) extends SemExp {
  override val t: TypeExp = elseClause match {
    case Some(s) => s.t.commonType(thenClause.t)
    case None    => Refer(BuiltinScope.typeDefs.lookupDirect("nil").get).commonType(thenClause.t)
  }
  val a = Array(condition.s, thenClause.s) ++ elseClause.map(_.s).toArray
  override def s = S("If", a: _*)
}
case class TagOp(pos: MExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
  override def s = S("Tag", S(pos.asInstanceOf[CallExp].args.head.asInstanceOf[QlyString].value))
}
case class GotoOp(pos: MExp, goto: mutable.ArrayBuffer[SemPos]) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nothing").get)
  override def s: S = S("Goto", S(pos.asInstanceOf[CallExp].args.head.asInstanceOf[QlyString].value))
}
case class CatchOp(pos: MExp, catchLabel: SymbolValue, t: TypeExp) extends SemExp {
  override def s: S = ???
}
case class ThrowOp(pos: MExp, catchLabel: SemExp, v: SemExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nothing").get)
  override def s: S = ???
}
case class BlockOp(pos: MExp, body: IndexedSeq[SemExp]) extends SemExp {
  override val t: TypeExp = if (body.isEmpty) Refer(BuiltinScope.typeDefs.lookupDirect("nil").get) else body.last.t
  override def s: S = S("Block", body.map(_.s): _*)
}
case class NewOp(pos: MExp, t: TypeExp, v: SemExp) extends SemExp {
  override def s = S("New", S(t.toString), v.s)
}
case class ToOp(pos: MExp, t: TypeExp, v: SemExp) extends SemExp {
  override def s = S("To", S(t.toString), v.s)
}
case class ReturnOp(pos: MExp, v: Option[SemExp]) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nothing").get)
  override def s = S("Return", v.map(_.s).toArray: _*)
}
case class DefOp(pos: MExp, varDef: VarDef, v: Option[SemExp]) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
  override def s = S("Def", Array(S(varDef.name), S(varDef.t.toString)) ++ v.map(_.s).toArray: _*)
}
case class SetOp(pos: MExp, location: SetLocation, v: SemExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
  override def s = S("Set", location.s, v.s)
}
case class FunOp(pos: MExp, t: FunType, paramNames: Seq[SymbolValue], body: SemTree) extends SemExp {
  val a = Array(S(t.toString), S("ParamNames", paramNames.map(n => S(n.value)): _*), S("FunBody", body.exps.map(_.s): _*))
  override def s = S("Fun", a: _*)
}
case class NoOp(pos: MExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
  override def s = S("NoOp")
}

trait SetLocation {
  def s: S
}
case class SetLocationVar(varDef: VarDef) extends SetLocation {
  override def s = S("Location", S(varDef.name))
}
case class SetLocationArrayAccess(arrayLocation: SemExp, index: SemExp) extends SetLocation {
  override def s = S("Location", arrayLocation.s, index.s)
}
case class SetLocationStructAccess(structLocation: SemExp, field: StructField) extends SetLocation {
  override def s = S("Location", structLocation.s, S(field.name))
}
case class ErrorSetLocation(error: SemanticError, mexp: MExp) extends SetLocation {
  override def s = S("Location", S("Error"))
}

// Ignore location, for match SemTree in SemAnalyzer tests
case class S(op: String, args: S*)
