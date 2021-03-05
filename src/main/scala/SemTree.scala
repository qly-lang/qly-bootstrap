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
}

case class SemErrorExp(error: SemanticError, pos: MExp) extends SemExp {
  override val t = ErrorType
}

trait Literal extends SemExp {
  val pos: Positional
}
case class NilLiteral(pos: Positional) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
}
case class Int32Literal(pos: QlyInt32) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("int32").get)
}
case class Int64Literal(pos: QlyInt64) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("int64").get)
}
case class Int128Literal(pos: QlyInt128) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("int128").get)
}
case class BigIntLiteral(pos: QlyBigInt) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("bigint").get)
}
case class UInt32Literal(pos: QlyUInt32) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("uint32").get)
}
case class UInt64Literal(pos: QlyUInt64) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("uint64").get)
}
case class UInt128Literal(pos: QlyUInt128) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("uint128").get)
}
case class BigUIntLiteral(pos: QlyBigUInt) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("biguint").get)
}
case class Float64Literal(pos: QlyFloat64) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("float64").get)
}
case class DecimalLiteral(pos: QlyDecimal) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("decimal").get)
}
case class StringLiteral(pos: QlyString) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("string").get)
}
case class BoolLiteral(pos: QlySymbol) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("bool").get)
}
case class SymbolLiteral(pos: QlySymbol) extends Literal {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("symbol").get)
}
case class ArrayExp(pos: Positional, t: TypeExp, elems: Seq[SemExp]) extends SemExp

case class VarRef(pos: QlySymbol, varDef: VarDef) extends SemExp {
  override val t: TypeExp = varDef.t
}
case class FunCall(pos: CallExp, f: SemExp, args: Seq[SemExp]) extends SemExp {
  override val t: TypeExp = f.t.asInstanceOf[FunType].returnType
}
case class ArrayAccess(pos: CallExp, a: SemExp, idx: SemExp) extends SemExp {
  override val t: TypeExp = a.t.asInstanceOf[ArrayType].elemType
}
case class StructFieldAccess(pos: MExp, a: SemExp, field: SymbolValue) extends SemExp {
  override val t: TypeExp = a.t.asInstanceOf[StructType].fields.find(fi => fi.name == field.value).get.t
}
case class IfOp(pos: MExp, condition: SemExp, thenClause: SemExp, elseClause: Option[SemExp]) extends SemExp {
  override val t: TypeExp = elseClause match {
    case Some(s) => s.t.commonType(thenClause.t)
    case None    => Refer(BuiltinScope.typeDefs.lookupDirect("nil").get).commonType(thenClause.t)
  }
}
case class TagOp(pos: MExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
}
case class GotoOp(pos: MExp, goto: mutable.ArrayBuffer[SemPos]) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nothing").get)
}
case class CatchOp(pos: MExp, catchLabel: SymbolValue, t: TypeExp) extends SemExp
case class ThrowOp(mexp: MExp, catchLabel: SemExp, v: SemExp)
case class BlockOp(pos: MExp, body: IndexedSeq[SemExp]) extends SemExp {
  override val t: TypeExp = if (body.isEmpty) Refer(BuiltinScope.typeDefs.lookupDirect("nil").get) else body.last.t
}
case class NewOp(pos: MExp, t: TypeExp, v: SemExp) extends SemExp
case class ToOp(pos: MExp, t: TypeExp, v: SemExp) extends SemExp
case class ReturnOp(pos: MExp, v: Option[SemExp]) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nothing").get)
}
case class DefOp(pos: MExp, varDef: VarDef, v: Option[SemExp]) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
}
case class SetOp(pos: MExp, location: SetLocation, v: SemExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
}
case class FunOp(pos: MExp, t: FunType, paramNames: Seq[SymbolValue], body: SemTree) extends SemExp
case class NoOp(pos: MExp) extends SemExp {
  override val t: TypeExp = Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
}

trait SetLocation
case class SetLocationVar(varDef: VarDef) extends SetLocation
case class SetLocationArrayAccess(arrayLocation: SemExp, index: SemExp) extends SetLocation
case class SetLocationStructAccess(structLocation: SemExp, field: StructField) extends SetLocation
case class ErrorSetLocation(error: SemanticError, mexp: MExp) extends SetLocation
