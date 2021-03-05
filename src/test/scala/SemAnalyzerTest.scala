import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class SemAnalyzerTest extends FreeSpec {
  def analyze(code: String) = {
    val ast = Parser(code)
    val typeAnalyzeResult = new TypeAnalyzer(ast).analyze
    assert(typeAnalyzeResult.errors.isEmpty)
    val symbolTable = typeAnalyzeResult.symbolTable
    new SemAnalyzer(ast, symbolTable).analyze
  }
}
