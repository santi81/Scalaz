
object FoldLeftList {
  def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
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




def sum[A: Monoid](xs: List[A]): A = {

  val m = implicitly[Monoid[A]]
  FoldLeftList.foldLeft(xs, m.mzero, m.mappend)

}
sum(List(1, 2, 3, 4))
sum(List("a", "b", "c"))
implicit val multiMonoid: Monoid[Int] = new Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a * b
  def mzero: Int = 1
}

sum(List(1, 2, 3, 4))

case class CookieBox(count: Int)
object CookieBox {
  implicit val CookieBoxMonoid = new Monoid[CookieBox] {
    val mzero = CookieBox(0)
    def mappend(i: CookieBox, j: CookieBox) = CookieBox(i.count + j.count)
  }
}

def howMany[A:Monoid](gm:A,gp:A):A = {
  val m = implicitly[Monoid[A]]
  m.mappend(gm,gp)
}