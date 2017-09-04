package Foldable

import scala.language.higherKinds

import Monoid._

trait Foldable[F[_]] {
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(Function.uncurried(m.compose))
}

object instances {
  implicit val listsIsFoldable = new Foldable[List] {
    override def foldLeft[A,B](as: List[A])(z:B)(f:(B,A)=>B):B = as match {
      case Nil => z
      case h::t => f(foldLeft(t)(z)(f),h)
    }
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldLeft(as)(z)((b : B, a : A) => f(a,b))
    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid.Monoid[B]): B = {
      foldLeft(as.map(f))(mb.zero)(Function.uncurried(mb.compose))
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def toList[A](t : Tree[A]) : List[A] = t match {
    case Leaf(value) => List(value)
    case Branch(left, right) => toList(left) ++ toList(right)
  }

  implicit val treeIsFoldable = new Foldable[Tree] {
    override def foldLeft[A,B](as : Tree[A])(z : B)(f : (B,A)=>B) : B = {
      toList(as).foldLeft(z)(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
      toList(as).map(f).foldLeft(mb.zero)(Function.uncurried(mb.compose))
    }
    
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???
  }
}