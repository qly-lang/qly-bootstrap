sealed trait TypeExp {
  def isSuper(other: TypeExp): Boolean =
    other.getClass == classOf[Refer] && {
      val o = other.asInstanceOf[Refer]
      o.to.name == "nothing" && o.to.scope == BuiltinScope
    }
  def isSuperOrSame(other: TypeExp): Boolean = isSuper(other) || this == other
  def commonType(other: TypeExp): TypeExp = {
    if (isSuperOrSame(other)) this
    else if (other.isSuper(this)) other
    else if (getClass == classOf[Refer] && other.getClass == classOf[Refer]) {
      Refer(
        asInstanceOf[Refer].superTypeList
          .find(p => other.asInstanceOf[Refer].superTypeList.contains(p))
          .getOrElse(BuiltinScope.typeDefs.lookupDirect("any").get)
      )
    } else {
      Refer(BuiltinScope.typeDefs.lookupDirect("any").get)
    }
  }
}

case class FunType(params: Seq[TypeExp], returnType: TypeExp) extends TypeExp {
  override def isSuper(other: TypeExp): Boolean = {
    if (other.getClass == classOf[FunType]) {
      val otherFun = other.asInstanceOf[FunType]
      val paramPairs = params zip otherFun.params
      return params.length == otherFun.params.length && paramPairs.forall(pair => {
        val param = pair._1
        val otherParam = pair._2
        otherParam.isSuperOrSame(param)
      }) && returnType.isSuperOrSame(
        otherFun.returnType
      ) && this != otherFun
    }
    super.isSuper(other)
  }
}
case class RangeType(start: BigInt, end: BigInt) extends TypeExp {
  override def isSuper(other: TypeExp): Boolean = {
    if (other.getClass == classOf[RangeType]) {
      val otherRange = other.asInstanceOf[RangeType]
      return otherRange.start >= start && otherRange.end <= end && this != otherRange
    }
    super.isSuper(other)
  }
}
case class ArrayType(elemType: TypeExp) extends TypeExp {
  override def isSuper(other: TypeExp): Boolean = {
    if (other.getClass == classOf[ArrayType]) {
      val otherArray = other.asInstanceOf[ArrayType]
      return elemType.isSuper(otherArray.elemType)
    }
    super.isSuper(other)
  }
}
case class StructType(fields: Seq[StructField]) extends TypeExp {
  override def isSuper(other: TypeExp): Boolean = {
    if (other.getClass == classOf[StructType]) {
      val otherStruct = other.asInstanceOf[StructType]
      val fieldPairs = fields zip otherStruct.fields
      return fields.length == otherStruct.fields.length && fieldPairs.forall(pair =>
        pair._1.name == pair._2.name && pair._1.t.isSuperOrSame(pair._2.t)
      ) && this != otherStruct
    }
    super.isSuper(other)
  }
}
case class StructField(name: String, t: TypeExp)
case class OpType(
    params: Option[Seq[TypeExp]] = None,
    returnType: Option[TypeExp] = None
) extends TypeExp {
  override def isSuper(other: TypeExp): Boolean = {
    if (other.getClass == classOf[OpType]) {
      val otherOp = other.asInstanceOf[OpType]
      if (params.isDefined && returnType.isDefined && otherOp.params.isDefined && otherOp.returnType.isDefined) {
        val paramPairs = params.get zip otherOp.params.get
        return params.get.length == otherOp.params.get.length && paramPairs.forall {
          case (param, otherParam) => otherParam.isSuperOrSame(param)
        } && returnType.get.isSuperOrSame(
          otherOp.returnType.get
        ) && this != otherOp
      }
    }
    super.isSuper(other)
  }
}
case class ExactType(value: MExp)
case object Untyped extends TypeExp
case object ErrorType extends TypeExp
case class Refer(to: TypeDef) extends TypeExp {
  override def isSuper(other: TypeExp): Boolean = {
    if (this == other) return false
    if (to.scope == BuiltinScope && to.name == "any") {
      return true
    }
    if (other.getClass == classOf[Refer]) {
      val otherRefer = other.asInstanceOf[Refer]
      def isSuperInner(child: TypeDef): Boolean = {
        if (child.parents.contains(to)) {
          return true
        }
        child.parents.exists(p => isSuperInner(p))
      }
      return isSuperInner(otherRefer.to)
    }
    super.isSuper(other)
  }

  def superTypeList: List[TypeDef] = {
    to.superTypeList
  }
}
