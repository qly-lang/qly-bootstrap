import scala.collection.mutable

class QlySem(val ast: AST) {
  val rootScope = new Scope()
  val scopes: mutable.Map[MExp, Scope] = mutable.Map[MExp, Scope]()
  val symbolScopes: mutable.Map[QlySymbol, Scope] =
    mutable.Map[QlySymbol, Scope]()

  def analyzeType() = {
    analyzeTypeMExps(ast.mexps, rootScope)
    this
  }

  def resolveVar() = {
    ast.mexps.foreach(m => resolveVarMExp(m, rootScope))
    this
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
      case CallExp(QlySymbol("f"), QlySymbol(fname) :: signature :: mexps) =>
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
            val td = new TypeDef(t.value, Some(mexp), d = PrimitiveType(t.value), scope = scope)
            scope.setType(t, td)
          case (t: QlySymbol) :: typeDef :: Nil =>
            // t[type1 typeDef]
            val d = scope.lookupTypeDirect(t)
            if (d.isDefined) {
              throw TypeAlreadyDefinedInScope(d.get)
            }
            val de = processType(typeDef, scope)
            val td = new TypeDef(t.value, Some(mexp), d = de, scope = scope)
            scope.setType(t, td)
          case ColonExp(t: QlySymbol, st: QlySymbol) :: Nil =>
            // t[type1:superType]
            if (scope.lookupType(t).isEmpty) {
              scope.setType(t, new TypeDef(t.value, Some(mexp), d = PrimitiveType(t.value), scope = scope))
            }
            val superType = scope.lookupType(st).getOrElse(throw UndefinedType(st))
            scope.lookupType(t).get.setSuper(superType)
          case ColonExp(t: QlySymbol, st: QlySymbol) :: typeDef :: Nil =>
            val d = scope.lookupTypeDirect(t)
            if (d.isDefined) {
              throw TypeAlreadyDefinedInScope(d.get)
            }
            val de = processType(typeDef, scope)
            val superType = scope.lookupType(st).getOrElse(throw UndefinedType(st))
            val td = new TypeDef(t.value, Some(mexp), d = de, scope = scope)
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
        referTo match {
          case None => throw UndefinedType(sym)
          case Some(referTo) =>
            if (referTo.scope == BuiltinScope) {
              referTo.d
            } else {
              Refer(to = referTo)
            }
        }
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
          case _                             => throw MalformedOp("Function signature should be either [param:type ...] or [param:type ...]:return-type")
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

  def resolveVarMExp(mexp: MExp, scope: Scope): TypeExp = {
    mexp match {
      case s: QlySymbol    => resolveVarQlySymbol(s, scope)
      case _: ColonExp     => throw MalformedExp(mexp, "Colon Exp can only appeared in var, type and function definition")
      case QlyArray(elems) => ArrayType(commonType(elems.map(mexp => resolveVarMExp(mexp, scope))))
      case c: CallExp      => resolveVarCallExp(c, scope)
      case i: QlyInt =>
        i match {
          case _: QlyInt8   => PrimitiveType("int8")
          case _: QlyInt16  => PrimitiveType("int16")
          case _: QlyInt32  => PrimitiveType("int32")
          case _: QlyInt64  => PrimitiveType("int64")
          case _: QlyInt128 => PrimitiveType("int128")
          case _: QlyBigInt => PrimitiveType("bigint")
        }
      case u: QlyUInt =>
        u match {
          case _: QlyUInt8   => PrimitiveType("uint8")
          case _: QlyUInt16  => PrimitiveType("uint16")
          case _: QlyUInt32  => PrimitiveType("uint32")
          case _: QlyUInt64  => PrimitiveType("uint64")
          case _: QlyUInt128 => PrimitiveType("uint128")
          case _: QlyBigUInt => PrimitiveType("biguint")
        }
      case r: QlyReal => {
        r match {
          case _: QlyFloat32 => PrimitiveType("float32")
          case _: QlyFloat64 => PrimitiveType("float64")
          case _: QlyDecimal => PrimitiveType("decimal")
        }
      }
      case _: QlyString => PrimitiveType("string")
      case _            => throw Unimplemented(mexp, "Unimplemented resolve type " + mexp.getClass.toString)
    }
  }

  def resolveVarQlySymbol(symbol: QlySymbol, scope: Scope) = {
    val d = scope.lookupVar(symbol)
    if (d.isDefined) {
      symbolScopes(symbol) = scope
      d.get.occurs.add(symbol)
      d.get.t
    } else {
      throw UndefinedVariable(symbol)
    }
  }

  def commonType(value: List[TypeExp]) = {
    value.reduce((t1, t2) => if (t1.isSuperOrSame(t2)) t1 else (t1.superTypeList.findFirst(t1s => t2.superTypeList.contains(t1s))))
  }

  def resolveVarCallExp(exp: CallExp, scope: Scope) = {}
}
