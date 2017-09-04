package Monad

import Functors.Functor
import scala.language.higherKinds

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a : A) : F[A]

  def flatMap[A,B](f : A => F[B])(a : F[A]) : F[B]

  override def fmap[A, B](f : A => B)(a : F[A]) : F[B] = 
    flatMap((x:A) => unit(f(x)))(a)

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = 
    flatMap((a:A) => fmap((b:B) => f(a,b))(mb))(ma)

  def sequence[A](lma: List[F[A]]): F[List[A]] = 
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))  
    
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = 
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = 
    (1 to n).foldLeft(unit(List.empty[A]))((p, _) => map2(ma, p)( _ :: _))
  
  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // example List[Int] -> (Int -> Option[Boolean]) -> Option[List[Int]]
  // example List[Int] -> (Int -> <List>[Boolean]) -> <List>[List[Int]]
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = 
    traverse(ms)((x:A) => unit(x))
}

object instances {

  import Monoid.Monoid
  import Monoid.extensions._
  import Monoid.instances.listIsMonoid
  import Monoid.instances.optionMonoid

  implicit val optionIsMonad = new Monad[Option] {
    override def unit[A](a: A) = Some(a)
    
    // override def flatMap[A,B](f : A => Option[B])(ma : Option[A]): Option[B] = 
    //   foldMap[A,Option[B]](ma, optionMonoid)(f)

    override def flatMap[A,B](f : A => Option[B])(a : Option[A]): Option[B] = a match {
      case None => None
      case Some(a) => f(a)
    }
  }

  implicit def eitherIsMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
     def flatMap[A, B](f: A => Either[E,B])(a: Either[E,A]): Either[E,B] = {
       a.right.flatMap(f)
     }

     def unit[A](a: A): Either[E,A] = Right(a)
  }

  val listIsMonad = new Monad[List] {
     override def unit[A](a:A) = List(a)
     override def flatMap[A,B](f : A => List[B])(a : List[A]) : List[B] = 
      foldMap[A,List[B]](a, listIsMonoid)(f)
  }

  type State[S,A] = (S => (S,A))

  class StateMonad[S] {
    type StateS[A] = State[S,A]

    def stateIsMonad = new Monad[StateS] {
      override def unit[A](a : A) : StateS[A] = (s : S) => (s, a)
      override def flatMap[A,B](f : A => StateS[B])(ma : StateS[A]) : StateS[B] = {
        (s : S) => {
          val (ns, value) = ma(s)
          f(value)(ns)
        }
      }
    }
  }

  class Reader[S] {
    type ReaderS[A] = S => A

    val readerIsMonad = new Monad[ReaderS] {
      override def unit[A](a:A):ReaderS[A] = (s : S) => a
      override def flatMap[A,B](f:A=>ReaderS[B])(ma : ReaderS[A]) : ReaderS[B] = {
        (s : S) => {
          f(ma(s))(s)
        }
      }
    }
  }

  type Writer[S] = (S,List[String])

  val writerIsMonad = new Monad[Writer] {
    override def unit[A](a : A) = (a, List())
    override def flatMap[A,B](f : A => Writer[B])(ma : Writer[A]) : Writer[B]= {
      val (value, written) = ma
      val (newValue, newWritten) = f(value)
      newValue -> (newWritten ++ written)
    }
  }
}

object extensions {
  def bind[F[_], A, B](a : F[A], f : A => F[B])(implicit m : Monad[F]) : F[B] = {
    m.flatMap(f)(a)
  }
}

object Test {
  import instances._
  import extensions._

  val a : Option[Int] = Some(1)
  val b : Option[Int] = Some(2)

  val c = bind(a, (x:Int) => bind(b, (y:Int) => Some(x+y)))

  val listOption : List[Option[Int]] = List(Some(1), Some(2))

  val options = optionIsMonad.sequence(listOption)

  val intList = List(1,2,3)
  val options2 = optionIsMonad.traverse(intList)(i => Some(s"${i}!"))

  def areAllEven(a : List[Int]) = optionIsMonad.traverse(a)(x => if (x % 2 == 0) Some(x) else None)

  val replicatedOption = optionIsMonad.replicateM(10, Some(1))
  val replicatedList = listIsMonad.replicateM(2, List(1,2,3))

  val readerStringInstance = new Reader[String]
  
  type MyReader[A] = Reader[String]#ReaderS[A]

  val getConfig : MyReader[String] = (s: String) => s

  def getInfoFromDb(config : String, name: String) : Int = 1
  val readerIsMonad = readerStringInstance.readerIsMonad
  def getInfoFromDbM(name : String) : MyReader[Int] = 
    readerIsMonad.flatMap((c:String) => readerIsMonad.unit(getInfoFromDb(c, name)))(getConfig)

  val result = readerIsMonad.flatMap((i:Int) => getInfoFromDbM("didier"))(readerIsMonad.unit(1))

  result("didier")

  val r = readerIsMonad.sequence(List(getInfoFromDbM("hey"), getInfoFromDbM("you")))
  val d = readerIsMonad.traverse(List("hey","you"))(getInfoFromDbM)
  val tartine = readerIsMonad.replicateM(5,getInfoFromDbM("hey"))

  def validateIsPosivite(i : Int) : Writer[Boolean] = if (i > 0) {
    writerIsMonad.unit(true)
  }  else (false, List("do not do this"))

  val writerFlow = writerIsMonad.flatMap((v : Boolean) => {
    writerIsMonad.unit(2)
  })(validateIsPosivite(1))
}