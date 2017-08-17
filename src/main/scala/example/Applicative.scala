package Applicative

import Functors._
import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  def ap[A,B](f : F[A => B])(a : F[A]) : F[B]
  def pure[A](a : A) : F[A]
}

object Applicative {
  def apply[F[_] : Applicative] : Applicative[F] = implicitly
}

object instances {
  import Functors.instances._

  implicit val optionIsApplicative = new Applicative[Option] {
    val instance = Functor[Option]

    override def ap[A,B](f : Option[ A => B ])(a : Option[A]) : Option[B] = f match {
      case None => None
      case Some(f) => instance.fmap(f)(a)
    }

    override def pure[A](a : A) : Option[A] = Some(a)

    // This is *really* ugly
    override def fmap[A,B](f : A => B)(a : Option[A]) : Option[B] = 
      instance.fmap(f)(a)
  }

  implicit val listIsApplicative = new Applicative[List] {
    val functorInstance = Functor[List]

    override def ap[A,B](f : List[ A => B ])(a : List[A]) : List[B] = f.flatMap(fp => a.map(fp))
    override def pure[A](a : A) : List[A] = List(a)
    override def fmap[A,B](f : A => B)(a : List[A]) : List[B] = functorInstance.fmap(f)(a)
  }
}

object extensions {
  import instances._

  implicit class ApplicativeExtensions2[F[_],A,B](f : F[A => B])(implicit applicativeInstance : Applicative[F]) {
    def <*>(a : F[A]) : F[B] = {
      applicativeInstance.ap(f)(a)
    }
  }

  // this does not work very well
  // compiler struggles to find the applicativeInstance
  // because A => B does not bear the functor type
  implicit class ApplicativeExtensions[A,B,F[_]](f : A => B)(implicit applicativeInstance : Applicative[F]) {
    def <&>(a : F[A]) : F[B] = {
      applicativeInstance.fmap(f)(a)
    }
  }
}

object Test {
  import instances.optionIsApplicative
  import extensions._

  val a : Option[Int] = Some(1)
  val op : Option[Int => Int => Int] = Some( x => y => x+y)
  val b = op <*> a <*> Some(2)

  val lol  = (x : Int) => (y : Int)  => x + y

  lol <&> Some(1) <*> Some(2)
}