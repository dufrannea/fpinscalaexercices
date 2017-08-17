import cats.Functor
import cats.instances.list._
import cats.instances.option._

object Lol {
  val listOption = List(Some(1),Some(2), Some(3))

  val funcListInstance = Functor[List]
  
  val myFuncInstance = Functor[List].compose[Option]
  
  myFuncInstance.map(listOption)(_ + 1)
  
}