import scala.collection.mutable

class SemAnalyzer(val ast: AST, val symbolTable: SymbolTable) {
  val symbolScopes: mutable.Map[QlySymbol, Scope] = mutable.Map[QlySymbol, Scope]()
  var errors = 0

  def analyze: SemTree = {
    new SemTree(ast.mexps.map(m => analyzeMExp(m, symbolTable.rootScope)).toVector, BuiltinScope)
  }

  def semError(error: SemanticError, mexp: MExp): SemErrorExp = {
    errors += 1
    SemErrorExp(error, mexp)
  }

  def analyzeMExp(mexp: MExp, scope: Scope): SemExp = {
    mexp match {
      case s: QlySymbol => analyzeQlySymbol(s, scope)
      case _: ColonExp  => semError(MalformedExp("Colon Exp can only appeared in var, type and function definition"), mexp)
      case a: QlyArray =>
        val es = a.value.map(mexp => analyzeMExp(mexp, scope))
        val t = commonType(es.map(_.t))
        ArrayExp(a, t, es)
      case c: CallExp => analyzeCallExp(c, scope)
      case i: QlyInt =>
        i match {
          case i: QlyInt32  => Int32Literal(i)
          case i: QlyInt64  => Int64Literal(i)
          case i: QlyInt128 => Int128Literal(i)
          case i: QlyBigInt => BigIntLiteral(i)
        }
      case u: QlyUInt =>
        u match {
          case u: QlyUInt32  => UInt32Literal(u)
          case u: QlyUInt64  => UInt64Literal(u)
          case u: QlyUInt128 => UInt128Literal(u)
          case u: QlyBigUInt => BigUIntLiteral(u)
        }
      case r: QlyReal => {
        r match {
          case r: QlyFloat64 => Float64Literal(r)
          case r: QlyDecimal => DecimalLiteral(r)
        }
      }
      case s: QlyString => StringLiteral(s)
      case _            => throw Unimplemented("Unimplemented analyze " + mexp.getClass.toString)
    }
  }

  def analyzeQlySymbol(symbol: QlySymbol, scope: Scope): SemExp = {
    val d = scope.lookupVar(symbol)
    if (d.isDefined) {
      symbolScopes(symbol) = scope
      d.get.occurs.add(symbol)
      VarRef(symbol, d.get)
    } else {
      semError(UndefinedVariable(symbol), symbol)
    }
  }

  def commonType(value: List[TypeExp]): TypeExp = {
    value.reduce((t1, t2) => {
      if (t1.isSuperOrSame(t2)) t1
      else if (t2.isSuper(t1)) t2
      else if (t1.getClass == classOf[Refer] && t2.getClass == classOf[Refer]) {
        Refer(
          t1.asInstanceOf[Refer]
            .superTypeList
            .find(p => t2.asInstanceOf[Refer].superTypeList.contains(p))
            .getOrElse(BuiltinScope.typeDefs.lookupDirect("any").get)
        )
      } else {
        Refer(BuiltinScope.typeDefs.lookupDirect("any").get)
      }
    })
  }

  def analyzeCallExp(exp: CallExp, scope: Scope): SemExp = {
    exp.value match {
      case _: CallExp => throw Unimplemented("consecutive call exp")
      case s: QlySymbol =>
        val name = analyzeQlySymbol(s, scope)
        if (name.t.getClass == classOf[OpType]) analyzeBuiltinOp(exp, scope)
        else if (name.t.getClass == classOf[ArrayType]) {
          val a = name.t.asInstanceOf[ArrayType]
          if (exp.args.length != 1) {
            return semError(MalformedExp("array should be indexed by only one expression"), exp)
          }
          val idx = analyzeMExp(exp.args.head, scope)
          if (Refer(BuiltinScope.typeDefs.lookupDirect("uint").get).isSuperOrSame(idx.t)) {
            ArrayAccess(exp, name, idx)
          } else {
            return semError(MalformedExp("array should be indexed by one of uint"), exp)
          }
        } else if (name.t.getClass == classOf[FunType]) {
          val f = name.t.asInstanceOf[FunType]
          if (f.params.last.getClass == classOf[ArrayType]) {
            val aT = f.params.last.asInstanceOf[ArrayType]
            if (f.params.length == 1 && exp.args.isEmpty) {
              // no argument, mean empty array as the only argument
              return FunCall(exp, name, Vector())
            }
            if (f.params.length > exp.args.length) {
              return semError(MalformedExp("function call doesn't provide enough arguments"), exp)
            }
            // by now args len >= 1, params len >= 1
            val args = (f.params.dropRight(1), exp.args).zipped.map((p, a) => {
              val arg = analyzeMExp(a, scope)
              if (p.isSuperOrSame(arg.t)) arg else semError(IncompatibleType(p), a)
            })
            if (f.params.length < exp.args.length) {
              val elems = exp.args
                .drop(f.params.length - 1)
                .map(a => {
                  val elem = analyzeMExp(a, scope)
                  if (aT.elemType.isSuperOrSame(elem.t)) elem else semError(IncompatibleType(aT.elemType), a)
                })
              val t = commonType(elems.map(_.t))
              val lastArg = ArrayExp(ASTPosition(exp.args.drop(f.params.length - 1).head.pos), t, elems)
              return FunCall(exp, name, args.appended(lastArg))
            } else {
              // params len == args len, last arg can either be array or elem
              val elem = analyzeMExp(exp.args.last, scope)
              if (aT.isSuperOrSame(elem.t)) {
                return FunCall(exp, name, args.appended(elem))
              } else if (aT.elemType.isSuperOrSame(elem.t)) {
                return FunCall(exp, name, args.appended(ArrayExp(ASTPosition(exp.args.last.pos), elem.t, Vector(elem))))
              }
              return semError(IncompatibleType(aT), exp.args.last)
            }
          } else {
            if (f.params.length != exp.args.length) {
              return semError(MalformedExp("function call doesn't provide correct number of arguments"), exp)
            }
            val args = (f.params, exp.args).zipped.map((p, a) => {
              val arg = analyzeMExp(a, scope)
              if (p.isSuperOrSame(arg.t)) arg else semError(IncompatibleType(p), a)
            })
            FunCall(exp, name, args)
          }
        } else {
          semError(MalformedExp("expect either operation, function call or array access"), exp)
        }
    }
  }

  def analyzeBuiltinOp(op: CallExp, scope: Scope): SemExp = {}
}
