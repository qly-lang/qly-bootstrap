import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class TypeAnalyzerTest extends FreeSpec {
  def analyze(code: String) = {
    new TypeAnalyzer(Parser(code)).analyze
  }

  "Define vars" - {
    "Primitive vars" in {
      val r = analyze("""
          |v[x:int 3]
          |v[y:string]
          |v[aaa:array[uint32]]
          |v[bbb:symbol 'x]
          |""".stripMargin)
      r.symbolTable.scopes.size shouldEqual (0)
      r.errors.shouldBe(empty)
      val env = r.symbolTable.rootScope.varDefs.env
      env.size shouldEqual (4)
      env("aaa").t shouldEqual (ArrayType(builtinType("uint32")))
      env("bbb").t shouldEqual (builtinType("symbol"))
      env("x").t shouldEqual (builtinType("int"))
      env("y").t shouldEqual (builtinType("string"))
    }

    "Arrays" in {
      val r = analyze("""
          |v[x:array[array[string]]]
          |v[y:[string]]
          |v[z:[[string]]]
          |v[a:[array[string]]]
          |v[b:array[[string]]]
          |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.shouldBe(empty)
      val env = r.symbolTable.rootScope.varDefs.env
      env.size shouldEqual (5)
      env("x").t shouldEqual (ArrayType(ArrayType(builtinType("string"))))
      env("y").t shouldEqual (ArrayType(builtinType("string")))
      env("z").t shouldEqual (ArrayType(ArrayType(builtinType("string"))))
      env("a").t shouldEqual (ArrayType(ArrayType(builtinType("string"))))
      env("b").t shouldEqual (ArrayType(ArrayType(builtinType("string"))))
    }

    "Structs" in {
      val r = analyze("""
          |v[x:struct[x:string]]
          |v[y:[y:string]]
          |v[z:[x:string y:[y:string] z:[string] a:[[x:string]]]]
          |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.shouldBe(empty)
      val env = r.symbolTable.rootScope.varDefs.env
      env.size shouldEqual (3)
      env("x").t.shouldEqual(StructType(List(StructField("x", builtinType("string")))))
      env("y").t.shouldEqual(StructType(List(StructField("y", builtinType("string")))))
      env("z").t.shouldEqual(
        StructType(
          List(
            StructField("x", builtinType("string")),
            StructField("y", StructType(List(StructField("y", builtinType("string"))))),
            StructField("z", ArrayType(builtinType("string"))),
            StructField("a", ArrayType(StructType(List(StructField("x", builtinType("string"))))))
          )
        )
      )
    }

    "Functions" in {
      val r = analyze("""
          |f[x []]
          |f[foo [x]]
          |f[bar [x:int y:string]:uint]
          |f[high [x:f[[]:nil] y:f[[]:uint]]:f[[uint]:uint]]
          |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.size shouldEqual (4)
      val env = r.symbolTable.rootScope.varDefs.env
      env.size.shouldEqual(4)
      env("x").t.shouldEqual(FunType(List(), Untyped))
      env("foo").t.shouldEqual(FunType(List(Untyped), Untyped))
      env("bar").t.shouldEqual(FunType(List(builtinType("int"), builtinType("string")), builtinType("uint")))
      env("high").t.shouldEqual(
        FunType(List(FunType(List(), builtinType("nil")), FunType(List(), builtinType("uint"))), FunType(List(builtinType("uint")), builtinType("uint")))
      )
      r.symbolTable.scopes(env("x").mexp.get).varDefs.env.shouldBe(empty)
      r.symbolTable.scopes(env("foo").mexp.get).varDefs.env("x").t.shouldEqual(Untyped)
      val barEnv = r.symbolTable.scopes(env("bar").mexp.get).varDefs.env
      barEnv("x").t.shouldEqual(builtinType("int"))
      barEnv("y").t.shouldEqual(builtinType("string"))
      val highEnv = r.symbolTable.scopes(env("high").mexp.get).varDefs.env
      highEnv("x").t.shouldEqual(FunType(List(), builtinType("nil")))
      highEnv("y").t.shouldEqual(FunType(List(), builtinType("uint")))
    }
  }

  def builtinType(name: String): Refer = {
    Refer(BuiltinScope.typeDefs.lookupDirect(name).get)
  }

  "Define types" - {
    "Simple type def" in {
      val r = analyze("""
                        |t[a int]
                        |t[b a]
                        |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.shouldBe(empty)
      r.symbolTable.rootScope.varDefs.env.shouldBe(empty)
      val env = r.symbolTable.rootScope.typeDefs.env
      env.size.shouldBe(2)
      env("a").d.shouldBe(Some(builtinType("int")))
      env("b").d.shouldBe(Some(Refer(env("a"))))
    }
  }
}
