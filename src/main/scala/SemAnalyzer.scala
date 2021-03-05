import scala.collection.mutable

class SemAnalyzer(val ast: AST, val symbolTable: SymbolTable) {
  val symbolScopes: mutable.Map[QlySymbol, Scope] = mutable.Map[QlySymbol, Scope]()
  val errors: mutable.ArrayBuffer[SemErrorExp] = mutable.ArrayBuffer()

  def analyze: SemAnalyzeResult = {
    new SemAnalyzeResult(new SemTree(ast.mexps.map(m => analyzeMExp(m, symbolTable.rootScope)).toVector, BuiltinScope), errors.toVector)
  }

  def semError(error: SemanticError, mexp: MExp): SemErrorExp = {
    val semError = SemErrorExp(error, mexp)
    errors += semError
    semError
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

  def analyzeFunDef(mexp: MExp, fname: QlySymbol, value: List[MExp], scope: Scope): FunOp = {
    val funType = scope.lookupType(fname).get.d.get.asInstanceOf[FunType]
    val params = value.head match {
      case ColonExp(v: QlyArray, _) => v
      case v: QlyArray              => v
    }
    val paramNames = params.value.map {
      case QlySymbol(n)              => SymbolValue(n)
      case ColonExp(QlySymbol(n), _) => SymbolValue(n)
    }
    val body = value.drop(1)
    val funScope = symbolTable.scopes(mexp)
    val semTree = new SemTree(body.map(m => analyzeMExp(m, funScope)).toVector, funScope)
    FunOp(mexp, funType, paramNames, semTree)
  }

  def analyzeBuiltinOp(op: CallExp, scope: Scope): SemExp = {
    op.value.asInstanceOf[QlySymbol].value match {
      case "t" =>
        val name = op.args.head match {
          case QlySymbol(n)              => n
          case ColonExp(QlySymbol(n), _) => n
        }
        val d = scope.varDefs.lookup(name).get
        DefOp(
          op,
          d,
          if (op.args.length == 2) { Some(analyzeMExp(op.args(1), scope)) }
          else None
        )
      case "f" =>
        val first = op.args.head
        val name = first match {
          case QlySymbol(n) => n
          case _            => ??? // Don't consider lambda for now
        }
        val funDef = analyzeFunDef(op, op.args.head.asInstanceOf[QlySymbol], op.args.drop(1), scope)
        val d = scope.varDefs.lookup(name).get
        DefOp(op, d, Some(funDef))
      case "t" => NoOp(op)
      case "b" => BlockOp(op, op.args.map(mexp => analyzeMExp(mexp, scope)).toVector)
      case "if" =>
        op.args match {
          case cond :: thenPart :: Nil             => IfOp(op, analyzeMExp(cond, scope), analyzeMExp(thenPart, scope), None)
          case cond :: thenPart :: elsePart :: Nil => IfOp(op, analyzeMExp(cond, scope), analyzeMExp(thenPart, scope), Some(analyzeMExp(elsePart, scope)))
          // TODO: since type analyzer consider all op right now, it should be able to catch error, so don't postpone error catching on this pass
          case _ => semError(MalformedOp("if should be if[condition then else] or if[condition then] form"), op)
        }
      case "new" =>
        op.args match {
          case t :: v :: Nil =>
            val ty = scope.mexpTypeExp(op)
            // TODO: check new feasibility
            NewOp(op, ty, analyzeMExp(v, scope))
        }
      case "tag" => {
        val tagOp = TagOp(op)
        scope.tags(op.args.head.asInstanceOf[QlySymbol].value) = Some(tagOp)
        tagOp
      }
      case "goto" =>
        val tag = op.args.head.asInstanceOf[QlySymbol].value
        scope.tags.get(tag) match {
          case Some(_) =>
            val goto = GotoOp(op, mutable.ArrayBuffer())
            scope.pendingGotos.append(goto)
            goto
          case None => semError(GotoTagNotExist(tag), op)
        }
      case "return" =>
        val arg = if (op.args.isEmpty) {
          None
        } else {
          Some(analyzeMExp(op.args.head, scope))
        }

        if (scope == symbolTable.rootScope) {
          if (arg.isEmpty) {
            semError(IncompatibleType(Refer(BuiltinScope.typeDefs.lookupDirect("int").get)), op)
          } else if (Refer(BuiltinScope.typeDefs.lookupDirect("int").get).isSuperOrSame(arg.get.t)) {
            ReturnOp(op, Some(arg.get))
          } else {
            semError(IncompatibleType(Refer(BuiltinScope.typeDefs.lookupDirect("int").get)), op)
          }
        } else {
          val scopeRetType = scope.retType.get
          val retValType = if (arg.isEmpty) {
            Refer(BuiltinScope.typeDefs.lookupDirect("nil").get)
          } else {
            arg.get.t
          }

          if (scopeRetType.isSuperOrSame(retValType)) {
            ReturnOp(op, arg)
          } else {
            semError(IncompatibleType(scopeRetType), op)
          }
        }
      case "set" =>
        val location = processLocation(op.args.head, scope)
        val value = analyzeMExp(op.args(1), scope)
        val t = location match {
          case ErrorSetLocation(error, mexp) => return semError(error, mexp)
          case SetLocationVar(v)             => v.t
          case SetLocationArrayAccess(a, _)  => a.t.asInstanceOf[ArrayType].elemType
          case SetLocationStructAccess(_, f) => f.t
        }
        if (t.isSuperOrSame(value.t)) {
          SetOp(op, location, value)
        } else {
          semError(IncompatibleType(t), op.args(1))
        }
      case "to" =>
        op.args match {
          case t :: v :: Nil =>
            val ty = scope.mexpTypeExp(op)
            // TODO: check `to` feasibility, once there's gf mechanism
            ToOp(op, ty, analyzeMExp(v, scope))
        }
    }
  }

  def processLocation(mexp: MExp, scope: Scope): SetLocation = {
    mexp match {
      case s: QlySymbol =>
        scope.lookupVar(s) match {
          case Some(v) => SetLocationVar(v)
          case None =>
            return ErrorSetLocation(UndefinedVariable(s), s)
        }
      case DotExp(v, d) =>
        val value = analyzeMExp(v, scope)
        value.t match {
          case struct: StructType =>
            if (!d.isInstanceOf[QlySymbol]) {
              return ErrorSetLocation(BadSetLocation("struct field should be a symbol"), d)
            }
            val field = d.asInstanceOf[QlySymbol]
            struct.fields.find(f => f.name == field.value) match {
              case Some(f) => SetLocationStructAccess(value, f)
              case None =>
                ErrorSetLocation(BadSetLocation("struct field does not exist"), d)
            }
          case _ =>
            ErrorSetLocation(BadSetLocation("expect struct type before dot"), v)
        }
      case CallExp(v, args) =>
        val value = analyzeMExp(v, scope)
        value.t match {
          case _: ArrayType =>
            if (args.length != 1) {
              return ErrorSetLocation(BadSetLocation("expect only one index"), mexp)
            }
            val index = analyzeMExp(args.head, scope)
            if (!Refer(BuiltinScope.typeDefs.lookupDirect("uint").get).isSuperOrSame(index.t)) {
              return ErrorSetLocation(BadSetLocation("array index need to be uint"), args.head)
            }
            SetLocationArrayAccess(value, index)
          case _ =>
            return ErrorSetLocation(BadSetLocation("expect array type before index access"), v)
        }
      case _ =>
        return ErrorSetLocation(BadSetLocation("expect variable, struct or array to set"), mexp)
    }
  }
}

class SemAnalyzeResult(val semTree: SemTree, val errors: Vector[SemErrorExp])
