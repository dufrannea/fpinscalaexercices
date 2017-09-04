package IO

import Monad.Monad

trait IO[A] { self => 
  def run() : A
  def flatMap[B](f : A => IO[B])(b : IO[A]) : IO[B] = new IO[B] {
    def run() : B = f(self.run()).run()
  }
  def map[B](f : A => B)(b : IO[B]) : IO[B] = new IO[B]{
    def run() = f(self.run())
  }
}

object Instances  {
  val ioMonad = new Monad[IO] {
    override def unit[A](a : A) : IO[A] = new IO[A]{ def run() : A = a }

    override def flatMap[A, B](f: A => IO[B])(a: IO[A]) : IO[B] = {
      f(a.run())
    }
  }
}