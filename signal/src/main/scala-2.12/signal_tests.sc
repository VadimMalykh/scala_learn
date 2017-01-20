import frp._

val a = Signal(2)
val b = Var(5)
val c = Signal(a() + b())

c()

b() = 18

c()

