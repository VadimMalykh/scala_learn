package frp

/**
  * Created by vadim on 20/01/2017.
  */
class Var[T](expr: => T) extends Signal[T](expr){
  def update(expr: => T): Unit = ???
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

