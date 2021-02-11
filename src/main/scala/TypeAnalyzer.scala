import scala.collection.mutable

class TypeAnalyzer(val ast: AST) {
  val rootScope = new Scope()
  val scopes: mutable.Map[MExp, Scope] = mutable.Map[MExp, Scope]()

  def analyze = {
    analyzeTypeMExps(ast.mexps, rootScope)
    new SymbolTable(rootScope, scopes.toMap)
  }

  def analyzeTypeMExps(mexps: Seq[MExp], scope: Scope) = {
    mexps.foreach(analyzeTypeMExpOut(_, scope))
    mexps.foreach(analyzeTypeMExpIn(_, scope))
  }

  def analyzeTypeMExpOut(mexp: MExp, scope: Scope) = {
    mexp match {
      case CallExp(QlySymbol("v"), List(ColonExp(QlySymbol(variable), t), _)) =>
        // v[variable : t _]
        val ty = processType(t, scope)
        scope.setVar(
          variable,
          new VarDef(variable, Some(mexp), ty, scope = scope)
        )
      case CallExp(QlySymbol("f"), QlySymbol(fname) :: signature :: _) =>
        // f[fname signature mexps]
        val t = signature match {
          case ColonExp(QlyArray(params), returnType) => FunType(processParamTypes(params, scope), processType(returnType, scope))
          case QlyArray(params)                       => FunType(processParamTypes(params, scope), Untyped)
        }
        scope.setVar(fname, new VarDef(fname, Some(mexp), t = t, scope = scope))
      case CallExp(QlySymbol("t"), args) =>
        // t[...]
        args match {
          case (t: QlySymbol) :: Nil =>
            // t[type1]
            val d = scope.lookupTypeDirect(t)
            if (d.isDefined) {
              throw TypeAlreadyDefinedInScope(d.get)
            }
            val td = new TypeDef(t.value, Some(mexp), scope = scope)
            scope.setType(t, td)
          case (t: QlySymbol) :: typeDef :: Nil =>
            // t[type1 typeDef]
            val d = scope.lookupTypeDirect(t)
            if (d.isDefined) {
              throw TypeAlreadyDefinedInScope(d.get)
            }
            val de = processType(typeDef, scope)
            val td = new TypeDef(t.value, Some(mexp), d = Some(de), scope = scope)
            scope.setType(t, td)
          case ColonExp(t: QlySymbol, st: QlySymbol) :: Nil =>
            // t[type1:superType]
            if (scope.lookupType(t).isEmpty) {
              scope.setType(t, new TypeDef(t.value, Some(mexp), scope = scope))
            }
            val superType = scope.lookupType(st).getOrElse(throw UndefinedType(st))
            scope.lookupType(t).get.setSuper(superType)
          case ColonExp(t: QlySymbol, st: QlySymbol) :: typeDef :: Nil =>
            // t[type1:superType typeDef]
            val d = scope.lookupTypeDirect(t)
            if (d.isDefined) {
              throw TypeAlreadyDefinedInScope(d.get)
            }
            val de = processType(typeDef, scope)
            val superType = scope.lookupType(st).getOrElse(throw UndefinedType(st))
            val td = new TypeDef(t.value, Some(mexp), d = Some(de), scope = scope)
            td.setSuper(superType)
            scope.setType(t, td)
          case _ => throw MalformedOp(mexp, "t should be t[type], t[type:supertype], t[type typedef] or t[type:supertype typedef]")
        }
    }
  }

  def processParamTypes(params: List[MExp], scope: Scope): List[TypeExp] = {
    params.map(param => {
      if (param.getClass != classOf[ColonExp]) {
        throw MalformedOp(param, "expect param:type in param lists")
      }
      processType(param.asInstanceOf[ColonExp].col, scope)
    })
  }

  def processType(t: MExp, scope: Scope): TypeExp = {
    t match {
      case sym: QlySymbol =>
        val referTo = scope.lookupType(sym)
        Refer(to = referTo.getOrElse(UndefinedType(sym)))
      case QlyArray(elems) =>
        if (elems.forall(elem => elem.getClass == classOf[ColonExp])) {
          StructType(
            elems.map(elem => processFieldType(elem.asInstanceOf[ColonExp], scope))
          )
        } else if (elems.length == 1) {
          ArrayType(processType(elems.head, scope))
        } else {
          throw MalformedType(
            t,
            "Pattern in [] need to be either a type of a list of field:type"
          )
        }
      case CallExp(QlySymbol("array"), args) =>
        if (args.length != 1) {
          throw MalformedType(
            t,
            "there should be one and only one type indicate as array element"
          )
        }
        ArrayType(processType(args.head, scope))
      case CallExp(QlySymbol("struct"), args) =>
        if (args.exists(elem => elem.getClass != classOf[ColonExp])) {
          throw MalformedType(t, "struct[] fields should be colon exps")
        }
        StructType(
          args.map(arg => processFieldType(arg.asInstanceOf[ColonExp], scope))
        )
      case CallExp(QlySymbol("f"), List(ColonExp(QlyArray(types), returnType))) =>
        FunType(
          types.map(t => processType(t, scope)),
          processType(returnType, scope)
        )
      case _ => throw MalformedType(t, "Unknown pattern of type")
    }
  }

  def processFieldType(colonExp: ColonExp, scope: Scope): StructField = {
    if (colonExp.value.getClass != classOf[QlySymbol]) {
      throw MalformedType(colonExp, "Struct field name need to be symbol")
    }
    StructField(
      colonExp.value.asInstanceOf[QlySymbol].value,
      processType(colonExp.col, scope)
    )
  }

  def analyzeTypeMExpIn(mexp: MExp, scope: Scope) = {
    mexp match {
      case CallExp(QlySymbol("f"), List(fname: QlySymbol, signature, mexps)) => {
        val newScope = new Scope(Some(scope), Some(mexp))
        scopes(mexp) = newScope
        signature match {
          case ColonExp(QlyArray(params), _) => processParamVars(params, newScope)
          case QlyArray(params)              => processParamVars(params, newScope)
          case _                             => throw MalformedOp(mexp, "Function signature should be either [param:type ...] or [param:type ...]:return-type")
        }
      }
    }
  }

  def processParamVars(params: List[MExp], scope: Scope) = {
    params.foreach(param => {
      if (param.getClass != classOf[ColonExp]) {
        throw MalformedOp(param, "function param should be colon exp")
      }
      val colonExp = param.asInstanceOf[ColonExp];
      val t = processType(colonExp.col, scope)
      if (colonExp.value.getClass != classOf[QlySymbol]) {
        throw MalformedOp(param, "function param name should be symbol")
      }
      val name = t.asInstanceOf[QlySymbol].value
      scope.setVar(name, new VarDef(name, Some(param), t = t, scope = scope))
    })
  }
}

class SymbolTable(val rootScope: Scope, val scopes: Map[MExp, Scope])
