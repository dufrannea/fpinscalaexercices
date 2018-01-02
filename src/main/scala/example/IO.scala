package IO

import Monad.Monad

trait NaiveIO[A] { self => 
  def run() : A
  def flatMap[B](f : A => NaiveIO[B])(b : NaiveIO[A]) : NaiveIO[B] = new NaiveIO[B] {
    def run(): B = f(self.run()).run()
  }
  def map[B](f : A => B)(b : NaiveIO[B]) : NaiveIO[B] = new NaiveIO[B]{
    def run() = f(self.run())
  }
}

object Instances  {
  val ioMonad = new Monad[NaiveIO] {
    override def unit[A](a : A): NaiveIO[A] = new NaiveIO[A] {
      def run() : A = a 
    }

    override def flatMap[A, B](f: A => NaiveIO[B])(a: NaiveIO[A]): NaiveIO[B] = {
      f(a.run())
    }
  }
}

trait IO[A] { self =>
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(self, f)

  @annotation.tailrec final def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Delay(thunk) => thunk()
    case Suspend(f) => run(f())
    case FlatMap(a, f) => {
      val torun = a match {
        case Return(ap) => f(ap)
        case Delay(thunk) => f(thunk())
        case Suspend(fs) => fs().flatMap(f)
        case FlatMap(ap, fp) => ap.flatMap(x => fp(x).flatMap(f))
      }
      run(torun)
    }
  }
}

object IO {
  def apply[A](f: () => A) = {
    Suspend(() => Return(f()))
  }
}

case class Return[A](a: A) extends IO[A]
case class Delay[A](thunk: () => A) extends IO[A]
case class Suspend[A](f: () => IO[A]) extends IO[A]
case class FlatMap[A,B](a: IO[A], f: A => IO[B]) extends IO[B]