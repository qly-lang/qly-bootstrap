import scala.collection.mutable

class Scope(
    val parent: Option[Scope] = Some(BuiltinScope),
    val mexp: Option[MExp] = None
) {
  val varDefs: EnvChain[String, VarDef] = new EnvChain(
    parent.map(parent => parent.varDefs)
  )
  val typeDefs: EnvChain[String, TypeDef] = new EnvChain(
    parent.map(parent => parent.typeDefs)
  )
  def lookupVar(sym: QlySymbol): Option[VarDef] =
    lookupVarDirect(sym).orElse(parent match {
      case Some(scope) => scope.lookupVar(sym)
      case None        => None
    })
  def lookupVarDirect(sym: QlySymbol) =
    varDefs.lookupDirect(sym.value) match {
      case Some(varDef) =>
        varDef.mexp match {
          case Some(mexp) => if (mexp.pos < sym.pos) Some(varDef) else None
          case None       => Some(varDef)
        }
      case None => None
    }
  def setVar(sym: QlySymbol, varDef: VarDef) = varDefs.set(sym.value, varDef)
  def setVar(name: String, varDef: VarDef) = varDefs.set(name, varDef)
  def lookupTypeDirect(sym: QlySymbol) =
    typeDefs.lookupDirect(sym.value) match {
      case Some(typeDef) =>
        typeDef.mexp match {
          case Some(mexp) => if (mexp.pos < sym.pos) Some(typeDef) else None
          case None       => Some(typeDef)
        }
      case None => None
    }
  def lookupType(sym: QlySymbol): Option[TypeDef] =
    lookupTypeDirect(sym).orElse(parent match {
      case Some(scope) => scope.lookupType(sym)
      case None        => None
    })
  def setType(sym: QlySymbol, typeDef: TypeDef) =
    typeDefs.set(sym.value, typeDef)
  def setType(name: String, typeDef: TypeDef) = typeDefs.set(name, typeDef)
}

class EnvChain[K, V](val parent: Option[EnvChain[K, V]] = None) {
  val env: mutable.Map[K, V] = mutable.Map[K, V]()
  def lookupDirect(name: K): Option[V] = env.get(name)
  def lookup(name: K): Option[V] =
    env
      .get(name)
      .orElse(parent match {
        case Some(envChain) => envChain.lookup(name)
        case None           => None
      })
  def set(name: K, v: V): Unit = env(name) = v
}

class VarDef(
    val name: String,
    val mexp: Option[MExp] = None,
    val t: TypeExp,
    val occurs: mutable.Set[QlySymbol] = mutable.Set(),
    val typeExpanded: TypeExp,
    val scope: Scope = BuiltinScope
)

class TypeDef(
    val name: String,
    val mexp: Option[MExp] = None,
    val d: TypeExp,
    val expanded: TypeExp,
    val parents: mutable.Set[TypeDef] = mutable.Set(),
    val children: mutable.Set[TypeDef] = mutable.Set(),
    val scope: Scope = BuiltinScope
) {
  def setSuper(s: TypeDef) = {
    parents.add(s)
    s.children.add(this)
  }
}

sealed trait TypeExp

case class PrimitiveType(name: String) extends TypeExp
case class FunType(params: Seq[TypeExp], returnType: TypeExp) extends TypeExp
case class RangeType(start: BigInt, end: BigInt) extends TypeExp
case class ArrayType(elemType: TypeExp) extends TypeExp
case class StructType(fields: Seq[StructField]) extends TypeExp
case class StructField(name: String, t: TypeExp)
case class OpType(
    params: Option[Seq[TypeExp]] = None,
    returnType: Option[TypeExp] = None
) extends TypeExp
case class ExactType(value: MExp)
case object Untyped extends TypeExp
case class Refer(to: TypeDef) extends TypeExp
