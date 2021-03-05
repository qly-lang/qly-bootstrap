trait CompilationError extends Throwable

trait SemanticError extends CompilationError

case class UndefinedVariable(variable: QlySymbol) extends SemanticError
case class UndefinedType(typeName: QlySymbol) extends SemanticError
case class IncompatibleType(expectedType: TypeExp) extends SemanticError
case class MalformedType(msg: String) extends SemanticError
case class MalformedOp(msg: String) extends SemanticError
case class BadSetLocation(msg: String) extends SemanticError
case class TypeAlreadyDefinedInScope(prevDef: TypeDef) extends SemanticError
case object TagExistsInScope extends SemanticError
case class GotoTagNotExist(tag: String) extends SemanticError
case class MalformedExp(msg: String) extends SemanticError
case class Unimplemented(msg: String) extends SemanticError
