

trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }

  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

object FoldLeft {
  implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
  }

  /*implicit val FoldLeftSeq: FoldLeft[Seq] = new FoldLeft[Seq] {
    def foldLeft[A, B](xs: Seq[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
  }*/
}

def sum[F[_]: FoldLeft, A: Monoid](xs: F[A]): A = {
  val m = implicitly[Monoid[A]]
  val fl = implicitly[FoldLeft[F]]
  fl.foldLeft(xs, m.mzero, m.mappend)
}

sum(List(1, 4, 3, 4))
sum(Seq(1,9,5))
