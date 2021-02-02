import scala.collection.mutable

object BuiltinScope extends Scope(parent = None) {
  def addType(name: String) = {
    setType(name, new TypeDef(name))
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
    new TypeDef("ascii-char", d = Some(asciiTypeExp))
  )

  val extendCharTypeExp = RangeType(0, 1114111)
  setType(
    "extend-char",
    new TypeDef(
      "extend-char",
      d = Some(extendCharTypeExp)
    )
  )

  val stringTypeExp = ArrayType(PrimitiveType("char"))
  setType(
    "string",
    new TypeDef("string", d = Some(stringTypeExp))
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
    ("true", Refer(typeDefs.lookupDirect("bool").get)),
    ("false", Refer(typeDefs.lookupDirect("bool").get)),
    ("v", OpType(returnType = Some(Refer(typeDefs.lookupDirect("symbol").get)))),
    ("f", OpType(returnType = Some(Refer(typeDefs.lookupDirect("symbol").get)))),
    ("t", OpType(returnType = Some(Refer(typeDefs.lookupDirect("symbol").get)))),
    ("block", OpType()),
    ("if", OpType()),
    ("while", OpType()),
    ("continue", OpType()),
    ("break", OpType()),
    ("return", OpType()),
    ("set", OpType()),
    (
      "+",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("number").get))
    ),
    (
      "-",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("number").get))
    ),
    (
      "*",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("number").get))
    ),
    (
      "/",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("number").get))
    ),
    (
      "**",
      FunType(
        List(Refer(typeDefs.lookupDirect("number").get), Refer(typeDefs.lookupDirect("number").get)),
        Refer(typeDefs.lookupDirect("number").get)
      )
    ),
    (
      "is",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("any").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      "=",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("any").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      "!=",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("any").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      ">",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      "<",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      ">=",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      "<=",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("number").get))), Refer(typeDefs.lookupDirect("bool").get))
    ),
    (
      ">>",
      FunType(
        List(Refer(typeDefs.lookupDirect("fixnum").get), Refer(typeDefs.lookupDirect("int32").get)),
        Refer(typeDefs.lookupDirect("fixnum").get)
      )
    ),
    (
      "<<",
      FunType(
        List(Refer(typeDefs.lookupDirect("fixnum").get), Refer(typeDefs.lookupDirect("int32").get)),
        Refer(typeDefs.lookupDirect("fixnum").get)
      )
    ),
    (
      "&",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("fixnum").get))),
        Refer(typeDefs.lookupDirect("fixnum").get)
      )
    ),
    (
      "|",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("fixnum").get))),
        Refer(typeDefs.lookupDirect("fixnum").get)
      )
    ),
    (
      "!",
      FunType(
        List(Refer(typeDefs.lookupDirect("fixnum").get)),
        Refer(typeDefs.lookupDirect("fixnum").get)
      )
    ),
    (
      "^",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("fixnum").get))),
        Refer(typeDefs.lookupDirect("fixnum").get)
      )
    ),
    (
      "and",
      OpType(
        Some(List(ArrayType(Refer(typeDefs.lookupDirect("bool").get)))),
        Some(Refer(typeDefs.lookupDirect("bool").get))
      )
    ),
    (
      "or",
      OpType(
        Some(List(ArrayType(Refer(typeDefs.lookupDirect("bool").get)))),
        Some(Refer(typeDefs.lookupDirect("bool").get))
      )
    ),
    (
      "not",
      OpType(
        Some(List(Refer(typeDefs.lookupDirect("bool").get))),
        Some(Refer(typeDefs.lookupDirect("bool").get))
      )
    ),
    (
      "length",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("any").get))), Refer(typeDefs.lookupDirect("uint").get))
    ),
    (
      "slice",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("any").get))),
        ArrayType(Refer(typeDefs.lookupDirect("any").get))
      )
    ),
    (
      "append",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("any").get))),
        ArrayType(Refer(typeDefs.lookupDirect("any").get))
      )
    ),
    (
      "concat",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("any").get)), ArrayType(Refer(typeDefs.lookupDirect("any").get))),
        ArrayType(Refer(typeDefs.lookupDirect("any").get))
      )
    ),
    (
      "del",
      FunType(
        List(ArrayType(Refer(typeDefs.lookupDirect("any").get)), Refer(typeDefs.lookupDirect("any").get)),
        Refer(typeDefs.lookupDirect("bool").get)
      )
    ),
    (
      "to",
      FunType(
        List(Refer(typeDefs.lookupDirect("any").get), Refer(typeDefs.lookupDirect("mexp").get)),
        Refer(typeDefs.lookupDirect("any").get)
      )
    ),
    ("shallow-copy", FunType(List(Refer(typeDefs.lookupDirect("any").get)), Refer(typeDefs.lookupDirect("any").get))),
    ("copy", FunType(List(Refer(typeDefs.lookupDirect("any").get)), Refer(typeDefs.lookupDirect("any").get))),
    ("r", FunType(List(Refer(typeDefs.lookupDirect("any").get)), Refer(typeDefs.lookupDirect("any").get))),
    (
      "ffi",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("any").get))), Refer(typeDefs.lookupDirect("any").get))
    ),
    (
      "syscall",
      FunType(List(ArrayType(Refer(typeDefs.lookupDirect("any").get))), Refer(typeDefs.lookupDirect("any").get))
    ),
    ("new", OpType()),
    ("for", OpType()),
    ("cond", OpType()),
    (
      "++",
      FunType(
        List(Refer(typeDefs.lookupDirect("number").get)),
        Refer(typeDefs.lookupDirect("number").get)
      )
    ),
    (
      "--",
      FunType(
        List(Refer(typeDefs.lookupDirect("number").get)),
        Refer(typeDefs.lookupDirect("number").get)
      )
    )
  )

  def setBuiltinVar(name: String, t: TypeExp) =
    setVar(name, new VarDef(name, t = t))

  builtinVars.map(s => setBuiltinVar(s._1, s._2))
}
