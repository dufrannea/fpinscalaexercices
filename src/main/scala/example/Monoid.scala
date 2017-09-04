package Monoid

trait Monoid[M] {
  def zero : M
  def compose(a : M)(b : M) : M
}

object instances {
  import scala.math.Numeric.IntIsIntegral

  /**
   * proof that string is a monoid
   */
  implicit val stringIsMonoid = new Monoid[String]{
    override def zero = ""
    override def compose(a : String)(b : String) = a + b
  }

  /**
   * proof that seq is a monoid
   */
  implicit def seqIsMonoid[A] = new Monoid[Seq[A]] {
     override def zero = Seq.empty[A]
     override def compose(a : Seq[A])( b : Seq[A]) = a ++ b
  }

  /**
   * proof that list is a monoid
   */
  implicit def listIsMonoid[A] = new Monoid[List[A]] {
     override def zero = List.empty[A]
     override def compose(a : List[A])( b : List[A]) = a ++ b
  }

  implicit def optionMonoid[A] = new Monoid[Option[A]] {
    override def zero = None
    override def compose(a: Option[A])(b:Option[A]) = a orElse b
  }

  implicit def endoMonoid[A] = new Monoid[A => A] {
    override def zero = identity
    override def compose(a : ( A => A))(b : (A => A)) : A => A = a compose b
  }
  /**
   * This allows to derive monoids instances for all 
   * numeric types
   */
  implicit def numericsAreMonoids[T](implicit evidence : Numeric[T]) = new Monoid[T] {
    override def zero = evidence.zero
    override def compose(a:T)(b:T) = evidence.plus(a,b)
  }
}

object extensions {

  implicit class MonoidWrapper[M](s : Seq[M])(implicit evidence : Monoid[M]) {
    /**
     * A seq of monoid is foldable.
     */
    def foldM() = s.fold(evidence.zero)(Function.uncurried(evidence.compose))
  }

  def dual[M](m : Monoid[M]) = new Monoid[M] {
    override def compose(a:M)(b:M) = m.compose(b)(a)
    override def zero: M = m.zero
  }

  /**
   * We have a list of non monoids but we can map to one
   */
  def foldMap[A,B](as: Seq[A], m: Monoid[B])(f: A => B): B  = {
    // Note that this is equivalent to 
    // as.map(f).fold(m.zero)(Function.uncurried(m.compose))
    new MonoidWrapper(as.map(f))(m).foldM()
  }

  /**
   * express foldLeft using foldMap
   */
  def foldLeft[A,B](s : List[A], o : B, f : (B, A) => B) : B= {
    import instances.endoMonoid

    type C = B => B
    // create a fp from A to (B => B)
    val fp : A => C = (a : A) => (b : B) => f(b,a)

    val evidence : Monoid[C] = endoMonoid[B]

    foldMap[A, C](s, evidence)(fp)(o)
  }

  def foldRight[A,B](s : List[A], o : B, f : (A, B)=>B) : B = {
    import instances.endoMonoid
    foldMap(s, dual(endoMonoid[B]))((a : A) => (b : B) => f(a,b))(o)
  }

  /**
   * Implements a balanced fold
   */
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.size match {
    case 0 => m.zero
    case 1 => f(v(0))
    case size => val (l, r) = v.splitAt( size / 2)
      m.compose(foldMapV(l,m)(f))(foldMapV(r,m)(f))
  }

  def isOrderedMonoid = new Monoid[Option[Int]] {
    def zero = Some(Integer.MIN_VALUE)
    def compose(a : Option[Int])(b : Option[Int]) : Option[Int] = a match {
      case None => None
      case Some(ap) => b match {
        case Some(bp) if ap <= bp => b
        case _ => None
      }
    }
  }

  def isOrdered(v : IndexedSeq[Int]) : Boolean = v.foldLeft[Option[Int]](Some(Integer.MIN_VALUE))({
    case (None, _) => None
    case (Some(previous), current) => if (previous <= current) Some(current) else None
  }).isDefined

  def isOrdered2(v : List[Int]) : Boolean = {
    val foldResult = foldMap(v, isOrderedMonoid)(a => Some(a))
    foldResult.isDefined
  }

 def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
   override def zero = (ma.zero, mb.zero)
   override def compose(a : (A,B))(b : (A,B)) : (A,B) = 
    ma.compose(a._1)(b._1) -> mb.compose(a._2)(b._2)
 }

 def functionMonoid[A,B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
   override def zero = _ => mb.zero
   override def compose(fa : A => B)(fb: A => B) : (A => B) = 
    (x : A) => mb.compose(fa(x))(fb(x))
 }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def compose(a: Map[K, V])(b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.compose(a.getOrElse(k, V.zero))(b.getOrElse(k, V.zero)))
        }
  }
}

object WCMonoid {
  sealed trait WC 
  case class Stub(isEmpty : Boolean) extends WC
  case class Part(hasLeft : Boolean,  val words: Int, hasRight:Boolean) extends WC

  // ("ast qsk qskd s" "qsdf qsdf qsdf")
  // ("ast qsk qskd s" "qsdf") => not getting one more
  val instanceMonoidWC = new Monoid[WC] {
    override def compose(a : WC)(b: WC) : WC = a match {
      case Part(hasLeft, words, hasRight) => b match {
        case Part(bHasLeft, bWords, bHasRight) => 
          Part(hasLeft, words+bWords+ (if (hasRight || bHasLeft) 1 else 0) , bHasRight)
        case Stub(be) => Part(hasLeft, words, !be || hasRight)
      }
      case Stub(ae) => b match {
        case Part(bHasleft, bWords, bHasRight) => 
          Part(!ae || bHasleft, bWords, bHasRight)
        case Stub(be) => Stub(ae && be)
      }
    }
    override def zero = Stub(true)
  }

  def compute(s : String) : Int = {
    def f(x : Char) : WC = x match {
      case s if s.isWhitespace => Part(false,0,false)
      case a => Stub(false)
    }
    import extensions.foldMapV

    foldMapV(s.toIndexedSeq, instanceMonoidWC)(f) match {
      case Part(hasLeft, words, hasRight) => 
        words + (if (hasLeft) 1 else 0) + (if (hasRight) 1 else 0)
      case Stub(e) => if (e) 0 else 1
    }
  }
}

object Test {
  import instances._
  import extensions._

  val a : Seq[String] = for {
    i <- (1 to 10)
  } yield s"element ${i}"

  a.foldM()

  // implicit val intIsMonoid : Monoid[Int] = numericsAreMonoids[Int](scala.math.Numeric.IntIsIntegral)
  // new MonoidWrapper(List(1,2,3).toSeq)(intIsMonoid).foldM()
  List(1,2,3).toSeq.foldM()
}