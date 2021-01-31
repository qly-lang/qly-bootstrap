trait QlyCompilationError extends Throwable

trait QlySemanticError extends QlyCompilationError

case class UndefinedVariable(variable: QlySymbol) extends QlySemanticError
case class UndefinedType(typeName: QlySymbol) extends QlySemanticError
case class IncompatibleType(exp: MExp, expectedType: TypeExp) extends QlySemanticError
case class MalformedType(exp: MExp, msg: String) extends QlySemanticError
case class MalformedOp(exp: MExp, msg: String) extends QlySemanticError
case class TypeAlreadyDefinedInScope(prevDef: TypeDef) extends QlySemanticError
case class MalformedExp(exp: MExp, msg: String) extends QlySemanticError
case class Unimplemented(exp: MExp, msg: String) extends QlySemanticError
