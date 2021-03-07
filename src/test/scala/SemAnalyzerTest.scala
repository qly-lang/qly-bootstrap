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

  "Simple" - {
    "Literals" in {
      analyze("3").s shouldEqual (Vector(S("Int32", S("3"))))
      analyze("2147483648").s shouldEqual (Vector(S("Int64", S("2147483648"))))
      analyze("9223372036854775808").s shouldEqual (Vector(S("Int128", S("9223372036854775808"))))
      analyze("170141183460469231731687303715884105728").s shouldEqual (Vector(S("BigInt", S("170141183460469231731687303715884105728"))))
      analyze("""
          |0x0
          |0b10000000000000000000000000000000
          |0o1000000000000000000000
          |0x80000000000000000000000000000000
          |0x800000000000000000000000000000000
          |""".stripMargin).s shouldEqual (
        Vector(
          S("UInt32", S("0x0")),
          S("UInt32", S("0b10000000000000000000000000000000")),
          S("UInt64", S("0o1000000000000000000000")),
          S("UInt128", S("0x80000000000000000000000000000000")),
          S("BigUInt", S("0x800000000000000000000000000000000"))
        )
      )
      analyze("""
          |3.5
          |170141183460469231731687303715884105728170141183460469231731687303715884105728.2
          |""".stripMargin).s shouldEqual (Vector(
        S("Float64", S("3.5")),
        S("Decimal", S("170141183460469231731687303715884105728170141183460469231731687303715884105728.2"))
      ))
      analyze("""
            |"aaa"
            |true
            |false
            |nil
            |""".stripMargin).s shouldEqual (Vector(
        S("String", S("aaa")),
        S("VarRef", S("true")),
        S("VarRef", S("false")),
        S("VarRef", S("nil"))
      ))
    }

    "ArrayExp" in {
      analyze("""
          |[123 true]
          |[123 4]
          |[123 2147483648]
          |[123 170141183460469231731687303715884105728]
          |""".stripMargin).s shouldEqual (
        Vector(
          S("Array", S("Refer(TypeDef(any))"), S("Int32", S("123")), S("VarRef", S("true"))),
          S("Array", S("Refer(TypeDef(int32))"), S("Int32", S("123")), S("Int32", S("4"))),
          S("Array", S("Refer(TypeDef(fixint))"), S("Int32", S("123")), S("Int64", S("2147483648"))),
          S("Array", S("Refer(TypeDef(int))"), S("Int32", S("123")), S("BigInt", S("170141183460469231731687303715884105728")))
        )
      )
    }

    "ArrayAccess" in {
      val a = analyze("""
          |[123 true][0]
          |[[123] [123]][0][0]
          |""".stripMargin)
      a.s shouldEqual (
        Vector(
          S("ArrayAccess", S("Array", S("Refer(TypeDef(any))"), S("Int32", S("123")), S("VarRef", S("true"))), S("Int32", S("0"))),
          S(
            "ArrayAccess",
            S(
              "ArrayAccess",
              S(
                "Array",
                S("ArrayType(Refer(TypeDef(int32)))"),
                S("Array", S("Refer(TypeDef(int32))"), S("Int32", S("123"))),
                S("Array", S("Refer(TypeDef(int32))"), S("Int32", S("123")))
              ),
              S("Int32", S("0"))
            ),
            S("Int32", S("0"))
          )
        )
      )
    }

    "FunCall" in {
      analyze("""
          |+[]
          |+[3 4]
          |+[3 4 5]
          |""".stripMargin).s shouldEqual (
        Vector(
          S("FunCall", S("VarRef", S("+")), S("Array", S("Refer(TypeDef(number))"))),
          S("FunCall", S("VarRef", S("+")), S("Array", S("Refer(TypeDef(int32))"), S("Int32", S("3")), S("Int32", S("4")))),
          S("FunCall", S("VarRef", S("+")), S("Array", S("Refer(TypeDef(int32))"), S("Int32", S("3")), S("Int32", S("4")), S("Int32", S("5"))))
        )
      )
    }

//// Struct literal isn't accepted in type analysis pass
//    "StructFieldAccess" in {
//      analyze("""
//          |[a:3 b:4]
//          |""".stripMargin)
//    }
  }
}
