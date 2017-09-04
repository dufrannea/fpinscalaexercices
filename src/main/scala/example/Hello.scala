package example
import scala.language.higherKinds

object MeasureStreamList {
  def mapSeq(s : Seq[Int], n : Int, f : Int => Int) : Seq[Int] = {
    (1 to n).foldLeft(s)({ case (p, _) =>  p.map(f) })
  }

  val result1      = mapSeq((1 to 10000).toStream, 1000, _ + 1)
  lazy val result2 = mapSeq((1 to 10000), 1000, _ + 1)

  def measure(thunk : => Any) = {
    val begin =  System.currentTimeMillis()
    val result = thunk
    val end =  System.currentTimeMillis()
    println(s"elapsed : ${end - begin}")
  }

  measure(result1.toList)
  measure(result2.toList)
}

object PayrollSystemWithTypeclass {
  case class Employee(name: String, id: Long)
  
  trait PayrollProcessor[C[_], A] {
    def processPayroll(payees: Seq[A]): Either[String, Throwable]
  }
  
  case class USPayroll[A](payees: Seq[A])(implicit processor: PayrollProcessor[USPayroll, A]) {
    def processPayroll = processor.processPayroll(payees)
  } 
  case class CanadaPayroll[A](payees: Seq[A])(implicit processor: PayrollProcessor[CanadaPayroll, A]) {
    def processPayroll = processor.processPayroll(payees)
  }
}

object PayrollProcessors {
  import PayrollSystemWithTypeclass._
  implicit object USPayrollProcessor extends PayrollProcessor[USPayroll, Employee] {
    def processPayroll(payees: Seq[Employee]) = Left("us employees are processed")
  }
  
  implicit object CanadaPayrollProcessor extends PayrollProcessor[CanadaPayroll, Employee] {
    def processPayroll(payees: Seq[Employee]) = Left("canada employees are processed")
  }
}

object Foos {
  // not a higher order type
  trait CanFoo[A] {
    def foos(x: A): String
  }

  object CanFoo {
    def apply[A:CanFoo]: CanFoo[A] = implicitly
  }

  case class Wrapper(wrapped: String)

  implicit object WrapperCanFoo extends CanFoo[Wrapper] {
    def foos(x: Wrapper) = x.wrapped
  }

  def foo[A:CanFoo](thing: A) = CanFoo[A].foos(thing)

  foo(Wrapper("hi"))
}

object FunctorInstances {
  trait Tartine[F[_]] {
    def fmap[A,B](f : A => B)(a : F[A]) : F[B]
  }


  // implicit object OptionFunctor extends Functor[Option] {
  //   def fmap[A,B](f : A => B)(a : Option[A]) : Option[B] = a match {
  //     case None => None
  //     case Some(o) => Some(f(o))
  //   }
  // }

  implicit val optionIsFunctor : Tartine[Option] = new Tartine[Option] {
    def fmap[A,B](f : A => B)(a : Option[A]) : Option[B] = a match {
      case None => None
      case Some(o) => Some(f(o))
    }
  }
  
  def map[A[_], B, C](a : A[B])(f : B => C)(implicit functorInstance : Tartine[A]) : A[C] = {
    functorInstance.fmap(f)(a)
  }

  def map2[A[_] : Tartine, B, C](a : A[B])(f : B => C) : A[C] = {
    //val F = implicitly[Tartine[A]]
    val G = Tartine[A]
    G.fmap(f)(a)
  }
  
  object Tartine {
    def apply[A[_] : Tartine] : Tartine[A] = implicitly
  }

  implicit class FunctorWrappa[A[B], B]( a : A[B])(implicit functorInstance : Tartine[A]) {
    def mip[C](f : B => C)(implicit functorInstance : Tartine[A]) : A[C] = {
     functorInstance.fmap(f)(a)
    }
  }
}

object RunPayroll2 {
  import PayrollSystemWithTypeclass._
  import PayrollProcessors._
  
  import FunctorInstances._
  
  implicitly[Tartine[Option]].fmap((x:Int) => x + 1 )(Some(1))

  val a : Option[Int] = Some(1)
  a.mip(x => x + 1)
  def run = {
    val r = USPayroll(Vector(Employee("a", 1))).processPayroll
    println(r)    
  }

  def main(args: Array[String]): Unit = {
    import Monoid._
    import Monoid.instances._
    import Monoid.extensions._

    val a = Map( 1 -> 4, 3 -> 5)
    val b = Map( 1 -> 7, 2 -> 5)
    
    val myMapSeq = Seq(a,b)

    val intSumMonoid : Monoid[Map[Int,Int]] =
      mapMergeMonoid[Int,Int](numericsAreMonoids[Int])

    val listMapMonoid = mapMergeMonoid[Int,Seq[Int]](seqIsMonoid)
    val folded = foldMap(myMapSeq, listMapMonoid)(
      m => m.map({ case (k,v) => k -> List(v)})
    )

    val intProductMonoid = 
      productMonoid(numericsAreMonoids[Int],numericsAreMonoids[Int])

    def parseInt(a : String) : Option[Int] = None

    val parsingFunctions : Seq[String => Option[Int]] = Seq(parseInt _, parseInt _, parseInt _)

    implicit val evidence = functionMonoid[String, Option[Int]](optionMonoid[Int])

    val sumAllInputs : (String => Option[Int])= parsingFunctions.foldM()

    println(sumAllInputs("a"))

    import Monad.Test
    println(Test.c)

    println(Test.options) 
    println(Test.options2)

    println(Test.areAllEven(List(1,2,3)))
    println(Test.areAllEven(List(2,4)))
    println(Test.replicatedOption)

    println(Test.replicatedList)
  }
}
