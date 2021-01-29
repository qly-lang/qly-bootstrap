class A()
class B() extends A
val b = List(new B())
b.head.getClass == classOf[A]
b.head.getClass == classOf[B]

val f = (b: Seq[A]) => {
  b.head
}
f(b)