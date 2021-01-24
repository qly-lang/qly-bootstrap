trait QlyCompilationError extends Throwable

trait QlySemanticError extends QlyCompilationError

class UndefinedVariable(val variable: QlySymbol) extends QlySemanticError
class UndefinedType(val typeName: QlySymbol) extends QlySemanticError
class IncompatibleType(val exp: MExp, val expectedType: TypeExp)

