import scala.collection.mutable

class QlySem(val ast: AST) {
  val scopes: mutable.Map[MExp, Scope] = mutable.Map[MExp, Scope]()
  val symbolScopes: mutable.Map[QlySymbol, Scope] =
    mutable.Map[QlySymbol, Scope]()
}

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

object BuiltinScope extends Scope(parent = None) {
  def addType(name: String) = {
    val typeExp = new PrimitiveType(name)
    setType(
      name,
      new TypeDef(
        name,
        d = typeExp,
        expanded = typeExp
      )
    )
  }

  val types = Vector(
    "nil",
    "any",
    "symbol",
    "int",
    "uint",
    "int8",
    "int16",
    "int32",
    "int64",
    "int128",
    "fixint",
    "bigint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "uint128",
    "fixuint",
    "biguint",
    "fixnum",
    "real",
    "float32",
    "float64",
    "decimal",
    "number",
    "bool",
    "mexp",
    "char"
  )

  types.foreach(addType)

  val asciiTypeExp = new RangeType(0, 127)
  setType(
    "ascii-char",
    new TypeDef("ascii-char", d = asciiTypeExp, expanded = asciiTypeExp)
  )

  val extendCharTypeExp = new RangeType(0, 1114111)
  setType(
    "extend-char",
    new TypeDef(
      "extend-char",
      d = extendCharTypeExp,
      expanded = extendCharTypeExp
    )
  )

  val stringTypeExp = new ArrayType(new PrimitiveType("char"))
  setType(
    "string",
    new TypeDef("string", d = stringTypeExp, expanded = stringTypeExp)
  )

  def setBuiltinSuperType(child: String, parent: String) =
    typeDefs.lookup(child).get.setSuper(typeDefs.lookup(parent).get)

  val superTypes = Vector(
    ("fixint", "int"),
    ("bigint", "int"),
    ("fixuint", "uint"),
    ("biguint", "uint"),
    ("int8", "fixint"),
    ("int16", "fixint"),
    ("int32", "fixint"),
    ("int64", "fixint"),
    ("int128", "fixint"),
    ("uint8", "fixuint"),
    ("uint16", "fixuint"),
    ("uint32", "fixuint"),
    ("uint64", "fixuint"),
    ("uint128", "fixuint"),
    ("fixint", "fixnum"),
    ("fixuint", "fixnum"),
    ("ascii-char", "char"),
    ("extend-char", "char"),
    ("float32", "real"),
    ("float64", "real"),
    ("decimal", "real"),
    ("int", "number"),
    ("uint", "number"),
    ("fixnum", "number"),
    ("real", "number")
  )
  superTypes.map(s => setBuiltinSuperType(s._1, s._2))

  val builtinVars: Vector[(String, TypeExp)] = Vector(
    ("true", new PrimitiveType("bool"))
  )

  def setBuiltinVar(name: String, t: TypeExp) =
    setVar(name, new VarDef(name, t = t, typeExpanded = t))

  builtinVars.map(s => setBuiltinVar(s._1, s._2))
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

trait TypeExp

class PrimitiveType(val name: String) extends TypeExp
class FunType(val params: Seq[TypeExp], val returnType: TypeExp) extends TypeExp
class RangeType(val start: BigInt, val end: BigInt) extends TypeExp
class ArrayType(val elemType: TypeExp) extends TypeExp
class StructType(val fields: Seq[StructField]) extends TypeExp
class StructField(val name: String, val t: TypeExp)
class OpType(val params: Seq[TypeExp], val returnType: TypeExp) extends TypeExp
class ExactType(val value: MExp)
