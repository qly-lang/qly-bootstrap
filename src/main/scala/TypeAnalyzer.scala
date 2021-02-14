import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class TypeAnalyzer(val ast: AST) {
  val rootScope = new Scope()
  val scopes: mutable.Map[MExp, Scope] = mutable.Map[MExp, Scope]()
  val errors: mutable.ArrayBuffer[SemErrorExp] = mutable.ArrayBuffer()

  def semError(error: SemanticError, mexp: MExp): ErrorType.type = {
    errors.append(SemErrorExp(error, mexp))
    ErrorType
  }

  def analyze = {
    analyzeTypeMExps(ast.mexps, rootScope)
    new TypeAnalyzeResult(new SymbolTable(rootScope, scopes.toMap), errors.toVector)
  }

  def analyzeTypeMExps(mexps: Seq[MExp], scope: Scope): Unit = {
    mexps.foreach(analyzeTypeMExpOut(_, scope))
    mexps.foreach(analyzeTypeMExpIn(_, scope))
  }

  def analyzeTypeMExpOut(mexp: MExp, scope: Scope): Unit = {
    mexp match {
      case CallExp(QlySymbol("v"), ColonExp(QlySymbol(variable), t) :: _) =>
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
            val superType = scope
              .lookupType(st)
              .getOrElse({
                semError(UndefinedType(st), st)
                return
              })
            scope.lookupType(t).get.setSuper(superType)
          case ColonExp(t: QlySymbol, st: QlySymbol) :: typeDef :: Nil =>
            // t[type1:superType typeDef]
            val d = scope.lookupTypeDirect(t)
            if (d.isDefined) {
              throw TypeAlreadyDefinedInScope(d.get)
            }
            val de = processType(typeDef, scope)
            val superType = scope
              .lookupType(st)
              .getOrElse({
                semError(UndefinedType(st), st)
                return
              })
            val td = new TypeDef(t.value, Some(mexp), d = Some(de), scope = scope)
            td.setSuper(superType)
            scope.setType(t, td)
          case _ => semError(MalformedOp("t should be t[type], t[type:supertype], t[type typedef] or t[type:supertype typedef]"), mexp)
        }
      case _ => {}
    }
  }

  def processParamTypes(params: List[MExp], scope: Scope): List[TypeExp] = {
    params.map(param => {
      if (param.getClass == classOf[ColonExp]) {
        processType(param.asInstanceOf[ColonExp].col, scope)
      } else if (param.getClass == classOf[QlySymbol]) {
        Untyped
      } else {
        semError(MalformedOp("expect param or param:type in param lists"), param)
      }
    })
  }

  def processType(t: MExp, scope: Scope): TypeExp = {
    t match {
      case sym: QlySymbol =>
        val referTo = scope.lookupType(sym)
        referTo match {
          case Some(to) => Refer(to)
          case None     => semError(UndefinedType(sym), sym)
        }
      case QlyArray(elems) =>
        if (elems.forall(elem => elem.getClass == classOf[ColonExp])) {
          val fields = elems.map(arg => processFieldType(arg.asInstanceOf[ColonExp], scope))
          try {
            StructType(fields.map(_.get))
          } catch {
            case _ => semError(MalformedType("Some struct field malformed"), t)
          }
        } else if (elems.length == 1) {
          ArrayType(processType(elems.head, scope))
        } else {
          semError(
            MalformedType(
              "Pattern in [] need to be either a type of a list of field:type"
            ),
            t
          )
        }
      case CallExp(QlySymbol("array"), args) =>
        if (args.length != 1) {
          return semError(
            MalformedType(
              "there should be one and only one type indicate as array element"
            ),
            t
          )
        }
        ArrayType(processType(args.head, scope))
      case CallExp(QlySymbol("struct"), args) =>
        if (args.exists(elem => elem.getClass != classOf[ColonExp])) {
          return semError(MalformedType("struct[] fields should be colon exps"), t)
        }
        val fields = args.map(arg => processFieldType(arg.asInstanceOf[ColonExp], scope))
        try {
          StructType(fields.map(_.get))
        } catch {
          case _ => semError(MalformedType("Some struct field malformed"), t)
        }
      case CallExp(QlySymbol("f"), List(ColonExp(QlyArray(types), returnType))) =>
        FunType(
          types.map(t => processType(t, scope)),
          processType(returnType, scope)
        )
      case _ => semError(MalformedType("Unknown pattern of type"), t)
    }
  }

  def processFieldType(colonExp: ColonExp, scope: Scope): Try[StructField] = {
    if (colonExp.value.getClass != classOf[QlySymbol]) {
      Failure(semError(MalformedType("Struct field name need to be symbol"), colonExp))
    }
    Success(
      StructField(
        colonExp.value.asInstanceOf[QlySymbol].value,
        processType(colonExp.col, scope)
      )
    )
  }

  def analyzeTypeMExpIn(mexp: MExp, scope: Scope) = {
    mexp match {
      case CallExp(QlySymbol("f"), (fname: QlySymbol) :: signature :: mexps) => {
        val newScope = new Scope(Some(scope), Some(mexp))
        scopes(mexp) = newScope
        signature match {
          case ColonExp(QlyArray(params), _) => processParamVars(params, newScope)
          case QlyArray(params)              => processParamVars(params, newScope)
          case _                             => semError(MalformedOp("Function signature should be either [param:type ...] or [param:type ...]:return-type"), mexp)
        }
        analyzeTypeMExps(mexps, newScope)
      }
      case _ => {}
    }
  }

  def processParamVars(params: List[MExp], scope: Scope) = {
    params.foreach(param => {
      if (param.getClass == classOf[ColonExp]) {
        val colonExp = param.asInstanceOf[ColonExp];
        val t = processType(colonExp.col, scope)
        if (colonExp.value.getClass != classOf[QlySymbol]) {
          semError(MalformedOp("function param name should be symbol"), param)
        } else {
          val name = colonExp.value.asInstanceOf[QlySymbol].value
          scope.setVar(name, new VarDef(name, Some(param), t = t, scope = scope))
        }
      } else if (param.getClass == classOf[QlySymbol]) {
        val sym = param.asInstanceOf[QlySymbol]
        scope.setVar(sym.value, new VarDef(sym.value, Some(param), t = Untyped, scope = scope))
      } else {
        semError(MalformedOp("function param should be symbol or colon exp"), param)
      }
    })
  }
}

class SymbolTable(val rootScope: Scope, val scopes: Map[MExp, Scope])

class TypeAnalyzeResult(val symbolTable: SymbolTable, val errors: Vector[SemErrorExp])
