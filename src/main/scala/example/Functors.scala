package Functors

import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[A, B](f : A => B)(a : F[A]) : F[B]
}

object Functor {
  // this allows to remove the implicitly when
  // invoking a Functor instance
  // see extensions.applicative$
  def apply[A[_] : Functor] : Functor[A] = implicitly
}

object instances {
  /**
   * Instance can be a val
   */
  implicit val optionsAreFunctors = new Functor[Option]{
    override def fmap[A,B](f : A => B)(a : Option[A]) : Option[B] = a match {
      case None => None
      case Some(o) => Some(f(o))
    }
  }

  /**
   * Instance can be an object
   */
  implicit object listsAreFunctors extends Functor[List]{
    override def fmap[A,B](f : A => B)(a : List[A]) : List[B] = a.map(f)
  }
}

object extensions {
  // offer to any functor a fmap function
  implicit class FunctorWrapper[F[_], B]( a : F[B])(implicit functorInstance : Functor[F]) {
    def fmap[C](f : B => C)(implicit functorInstance : Functor[F]) : F[C] = {
     functorInstance.fmap(f)(a)
    }
  }

  // here we define the applicative <$> operator
  // we do not use the implicity notation thanks
  // to the Functor companion object' apply method
  def applicative$[A[_] : Functor, B, C]( f : B => C )( a : A[B] ) : A[C] = {
    Functor[A].fmap(f)(a)
  }
}

object Test {
  import instances._
  import extensions._

  val a : Option[Int] = Some(1)

  // notation allowed by 
  // extension method defined in functorwrapper
  a.fmap(x => x + 1)

  applicative$((x : Int) => x + 1)(a)

  // lists are functors as well
  val l : List[Int] = List(1)
  a.fmap(x => 2*x)
}

