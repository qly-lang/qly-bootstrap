import scala.collection.mutable

object BuiltinScope extends Scope(parent = None) {
  def addType(name: String) = {
    val typeExp = PrimitiveType(name)
    setType(
      name,
      new TypeDef(
        name,
        d = typeExp,
        expanded = typeExp
      )
    )
  }

  val types = Vector(
    "nil",
    "any",
    "symbol",
    "int",
    "uint",
    "int8",
    "int16",
    "int32",
    "int64",
    "int128",
    "fixint",
    "bigint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "uint128",
    "fixuint",
    "biguint",
    "fixnum",
    "real",
    "float32",
    "float64",
    "decimal",
    "number",
    "bool",
    "mexp",
    "char"
  )

  types.foreach(addType)

  val asciiTypeExp = RangeType(0, 127)
  setType(
    "ascii-char",
    new TypeDef("ascii-char", d = asciiTypeExp, expanded = asciiTypeExp)
  )

  val extendCharTypeExp = RangeType(0, 1114111)
  setType(
    "extend-char",
    new TypeDef(
      "extend-char",
      d = extendCharTypeExp,
      expanded = extendCharTypeExp
    )
  )

  val stringTypeExp = ArrayType(PrimitiveType("char"))
  setType(
    "string",
    new TypeDef("string", d = stringTypeExp, expanded = stringTypeExp)
  )

  def setBuiltinSuperType(child: String, parent: String) =
    typeDefs.lookup(child).get.setSuper(typeDefs.lookup(parent).get)

  val superTypes = Vector(
    ("fixint", "int"),
    ("bigint", "int"),
    ("fixuint", "uint"),
    ("biguint", "uint"),
    ("int8", "fixint"),
    ("int16", "fixint"),
    ("int32", "fixint"),
    ("int64", "fixint"),
    ("int128", "fixint"),
    ("uint8", "fixuint"),
    ("uint16", "fixuint"),
    ("uint32", "fixuint"),
    ("uint64", "fixuint"),
    ("uint128", "fixuint"),
    ("fixint", "fixnum"),
    ("fixuint", "fixnum"),
    ("ascii-char", "char"),
    ("extend-char", "char"),
    ("float32", "real"),
    ("float64", "real"),
    ("decimal", "real"),
    ("int", "number"),
    ("uint", "number"),
    ("fixnum", "number"),
    ("real", "number")
  )
  superTypes.map(s => setBuiltinSuperType(s._1, s._2))

  val builtinVars: Vector[(String, TypeExp)] = Vector(
    ("true", PrimitiveType("bool")),
    ("false", PrimitiveType("bool")),
    ("v", OpType(returnType = Some(PrimitiveType("symbol")))),
    ("f", OpType(returnType = Some(PrimitiveType("symbol")))),
    ("t", OpType(returnType = Some(PrimitiveType("symbol")))),
    ("block", OpType()),
    ("if", OpType()),
    ("while", OpType()),
    ("continue", OpType()),
    ("break", OpType()),
    ("return", OpType()),
    ("set", OpType()),
    (
      "+",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("number"))
    ),
    (
      "-",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("number"))
    ),
    (
      "*",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("number"))
    ),
    (
      "/",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("number"))
    ),
    (
      "**",
      FunType(
        List(PrimitiveType("number"), PrimitiveType("number")),
        PrimitiveType("number")
      )
    ),
    (
      "is",
      FunType(List(ArrayType(PrimitiveType("any"))), PrimitiveType("bool"))
    ),
    (
      "=",
      FunType(List(ArrayType(PrimitiveType("any"))), PrimitiveType("bool"))
    ),
    (
      "!=",
      FunType(List(ArrayType(PrimitiveType("any"))), PrimitiveType("bool"))
    ),
    (
      ">",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("bool"))
    ),
    (
      "<",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("bool"))
    ),
    (
      ">=",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("bool"))
    ),
    (
      "<=",
      FunType(List(ArrayType(PrimitiveType("number"))), PrimitiveType("bool"))
    ),
    (
      ">>",
      FunType(
        List(PrimitiveType("fixnum"), PrimitiveType("int32")),
        PrimitiveType("fixnum")
      )
    ),
    (
      "<<",
      FunType(
        List(PrimitiveType("fixnum"), PrimitiveType("int32")),
        PrimitiveType("fixnum")
      )
    ),
    (
      "&",
      FunType(
        List(ArrayType(PrimitiveType("fixnum"))),
        PrimitiveType("fixnum")
      )
    ),
    (
      "|",
      FunType(
        List(ArrayType(PrimitiveType("fixnum"))),
        PrimitiveType("fixnum")
      )
    ),
    (
      "!",
      FunType(
        List(PrimitiveType("fixnum")),
        PrimitiveType("fixnum")
      )
    ),
    (
      "^",
      FunType(
        List(ArrayType(PrimitiveType("fixnum"))),
        PrimitiveType("fixnum")
      )
    ),
    (
      "and",
      OpType(
        Some(List(ArrayType(PrimitiveType("bool")))),
        Some(PrimitiveType("bool"))
      )
    ),
    (
      "or",
      OpType(
        Some(List(ArrayType(PrimitiveType("bool")))),
        Some(PrimitiveType("bool"))
      )
    ),
    (
      "not",
      OpType(
        Some(List(PrimitiveType("bool"))),
        Some(PrimitiveType("bool"))
      )
    ),
    (
      "length",
      FunType(List(ArrayType(PrimitiveType("any"))), PrimitiveType("uint"))
    ),
    (
      "slice",
      FunType(
        List(ArrayType(PrimitiveType("any"))),
        ArrayType(PrimitiveType("any"))
      )
    ),
    (
      "append",
      FunType(
        List(ArrayType(PrimitiveType("any"))),
        ArrayType(PrimitiveType("any"))
      )
    ),
    (
      "concat",
      FunType(
        List(ArrayType(PrimitiveType("any")), ArrayType(PrimitiveType("any"))),
        ArrayType(PrimitiveType("any"))
      )
    ),
    (
      "del",
      FunType(
        List(ArrayType(PrimitiveType("any")), PrimitiveType("any")),
        PrimitiveType("bool")
      )
    ),
    (
      "to",
      FunType(
        List(PrimitiveType("any"), PrimitiveType("mexp")),
        PrimitiveType("any")
      )
    ),
    ("shallow-copy", FunType(List(PrimitiveType("any")), PrimitiveType("any"))),
    ("copy", FunType(List(PrimitiveType("any")), PrimitiveType("any"))),
    ("r", FunType(List(PrimitiveType("any")), PrimitiveType("any"))),
    (
      "ffi",
      FunType(List(ArrayType(PrimitiveType("any"))), PrimitiveType("any"))
    ),
    (
      "syscall",
      FunType(List(ArrayType(PrimitiveType("any"))), PrimitiveType("any"))
    ),
    ("for", OpType()),
    ("cond", OpType()),
    (
      "++",
      FunType(
        List(PrimitiveType("number")),
        PrimitiveType("number")
      )
    ),
    (
      "--",
      FunType(
        List(PrimitiveType("number")),
        PrimitiveType("number")
      )
    )
  )

  def setBuiltinVar(name: String, t: TypeExp) =
    setVar(name, new VarDef(name, t = t, typeExpanded = t))

  builtinVars.map(s => setBuiltinVar(s._1, s._2))
}
