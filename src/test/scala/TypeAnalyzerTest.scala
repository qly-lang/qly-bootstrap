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

    "Complex type def" in {
      val r = analyze("""
                        |t[a f[[int]:int]]
                        |t[b [int]]
                        |t[c array[int]]
                        |t[d [c:int]]
                        |t[e struct[e:int]]
                        |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.shouldBe(empty)
      r.symbolTable.rootScope.varDefs.env.shouldBe(empty)
      val env = r.symbolTable.rootScope.typeDefs.env
      env.size.shouldBe(5)
      env("a").d.shouldBe(Some(FunType(List(builtinType("int")), builtinType("int"))))
      env("b").d.shouldBe(Some(ArrayType(builtinType("int"))))
      env("c").d.shouldBe(Some(ArrayType(builtinType("int"))))
      env("d").d.shouldBe(Some(StructType(List(StructField("c", builtinType("int"))))))
      env("e").d.shouldBe(Some(StructType(List(StructField("e", builtinType("int"))))))
    }

    "Recursive type def" in {
      val r = analyze("""
                        |t[tree]
                        |t[data int]
                        |t[node struct[node:data left:tree right:tree]]
                        |t[leaf:tree data]
                        |t[node:tree]
                        |
                        |t[type1]
                        |t[type2:type1 [type1]]
                        |t[int:type1]
                        |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.shouldBe(empty)
      r.symbolTable.rootScope.varDefs.env.shouldBe(empty)
      val env = r.symbolTable.rootScope.typeDefs.env
      env.size.shouldBe(6)
      env("tree").d.shouldBe(None)
      env("data").d.shouldBe(Some(builtinType("int")))
      env("node").d.shouldBe(
        Some(StructType(List(StructField("node", Refer(env("data"))), StructField("left", Refer(env("tree"))), StructField("right", Refer(env("tree"))))))
      )
      env("leaf").d.shouldBe(Some(Refer(env("data"))))
      env("leaf").parents.size shouldBe (1)
      env("leaf").parents.should(contain(env("tree")))
      env("node").parents.size shouldBe (1)
      env("node").parents.should(contain(env("tree")))
      env("type1").d.shouldBe(None)
      env("type2").d.shouldBe(Some(ArrayType(Refer(env("type1")))))
      env("type2").parents.size shouldBe (1)
      env("type2").parents.should(contain(env("type1")))
      builtinType("int").to.parents.size shouldBe (2)
      builtinType("int").to.parents.should(contain(env("type1")))
      env("type1").children.size shouldBe (2)
      env("type1").children.should(contain(env("type2")))
      env("type1").children.should(contain(builtinType("int").to))
    }
  }

  "Var defs with defined type" in {
    val r = analyze("""
                      |t[x int]
                      |v[y:x]
                      |v[x:x 3]
                      |""".stripMargin)
    r.errors.shouldBe(empty)
    r.symbolTable.scopes.shouldBe(empty)
    val typeEnv = r.symbolTable.rootScope.typeDefs.env
    typeEnv.size.shouldEqual(1)
    typeEnv("x").d.shouldEqual(Some(builtinType("int")))
    val varEnv = r.symbolTable.rootScope.varDefs.env
    varEnv.size.shouldEqual(2)
    varEnv("x").t shouldEqual (Refer(typeEnv("x")))
    varEnv("y").t shouldEqual (Refer(typeEnv("x")))
  }

  "scopes" - {
    "Def in new scope" in {
      val r = analyze("""
                        |t[type1 int]
                        |t[foo int]
                        |v[var1:type1 3]
                        |v[var2:type1 4]
                        |f[foo [var1]
                        |  t[type1 string]
                        |  t[type2 foo]
                        |  t[type3 type1]
                        |  t[foo:type1]
                        |  v[var2:type2 true]
                        |  v[var3:type1]
                        |  v[var4:foo]]
                        |""".stripMargin)
      r.errors.shouldBe(empty)
      r.symbolTable.scopes.size shouldBe (1)
      val rootTypeEnv = r.symbolTable.rootScope.typeDefs.env
      rootTypeEnv.size.shouldEqual(2)
      rootTypeEnv("type1").d.shouldEqual(Some(builtinType("int")))
      rootTypeEnv("foo").d.shouldEqual(Some(builtinType("int")))
      val rootVarEnv = r.symbolTable.rootScope.varDefs.env
      rootVarEnv.size.shouldEqual(3)
      rootVarEnv("var1").t.shouldEqual(Refer(rootTypeEnv("type1")))
      rootVarEnv("var2").t.shouldEqual(Refer(rootTypeEnv("type1")))
      rootVarEnv("foo").t.shouldEqual(FunType(List(Untyped), Untyped))
      val fooScope = r.symbolTable.scopes(rootVarEnv("foo").mexp.get)
      val fooTypeEnv = fooScope.typeDefs.env
      val fooVarEnv = fooScope.varDefs.env
      fooTypeEnv.size.shouldEqual(3)
      fooTypeEnv("type1").d.shouldEqual(Some(builtinType("string")))
      rootTypeEnv("foo").parents.should(contain(fooTypeEnv("type1")))
      rootTypeEnv("foo").parents.size.shouldEqual(1)
      fooTypeEnv("type2").d.shouldEqual(Some(Refer(rootTypeEnv("foo"))))
      fooTypeEnv("type3").d.shouldEqual(Some(Refer(fooTypeEnv("type1"))))
      fooVarEnv.size.shouldEqual(4)
      fooVarEnv("var1").t.shouldEqual(Untyped)
      fooVarEnv("var2").t.shouldEqual(Refer(fooTypeEnv("type2")))
      fooVarEnv("var3").t.shouldEqual(Refer(fooTypeEnv("type1")))
      fooVarEnv("var4").t.shouldEqual(Refer(rootTypeEnv("foo")))

    }
  }

  "Errors" - {
    "inexist type in type def" in {
      val r = analyze("""
          |t[a what]
          |""".stripMargin)
      r.errors.size.shouldEqual(1)
      r.errors.head.error.getClass.shouldEqual(classOf[UndefinedType])
      r.errors.head.pos.shouldEqual(r.symbolTable.rootScope.typeDefs.env("a").mexp.get.asInstanceOf[CallExp].args(1))
    }

    "inexist type in var def" in {
      var r = analyze("""
          |v[a:what]
          |""".stripMargin)
      r.errors.size.shouldEqual(1)
      r.errors.head.error.getClass.shouldEqual(classOf[UndefinedType])
      r.errors.head.pos.shouldEqual(r.symbolTable.rootScope.varDefs.env("a").mexp.get.asInstanceOf[CallExp].args.head.asInstanceOf[ColonExp].col)
    }
  }
}
