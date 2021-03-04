import scala.collection.mutable

class Scope(
    val parent: Option[Scope] = Some(BuiltinScope),
    val mexp: Option[MExp] = None,
    val retType: Option[TypeExp] = None
) {
  val varDefs: EnvChain[String, VarDef] = new EnvChain(
    parent.map(parent => parent.varDefs)
  )
  val typeDefs: EnvChain[String, TypeDef] = new EnvChain(
    parent.map(parent => parent.typeDefs)
  )
  // Lambda doesn't have a name, map entire mexp to corresponding FunType
  val lambdaTypes: mutable.Map[MExp, FunType] = mutable.Map()
  // In new[type exp] and to[type exp], map the MExp "type" to analyzed TypeExp
  val mexpTypeExp: mutable.Map[MExp, TypeExp] = mutable.Map()
  val tags: mutable.Map[String, Option[TagOp]] = mutable.Map()
  val pendingGotos: mutable.ArrayBuffer[GotoOp] = mutable.ArrayBuffer()

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
    val scope: Scope = BuiltinScope
)

class TypeDef(
    val name: String,
    val mexp: Option[MExp] = None,
    val d: Option[TypeExp] = None,
    val parents: mutable.Set[TypeDef] = mutable.Set(),
    val children: mutable.Set[TypeDef] = mutable.Set(),
    val scope: Scope = BuiltinScope
) {
  def setSuper(s: TypeDef) = {
    parents.add(s)
    s.children.add(this)
  }

  def superTypeList: List[TypeDef] = {
    val ps: mutable.Queue[TypeDef] = mutable.Queue(this)
    val res = mutable.ArrayBuffer[TypeDef]()
    while (ps.nonEmpty) {
      val p = ps.dequeue()
      ps.addAll(p.parents)
      res.append(p)
    }
    res.toList
  }
}
