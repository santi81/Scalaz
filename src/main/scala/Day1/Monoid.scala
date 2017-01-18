package Day1


trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}


object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Option] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

object Test extends App{

  //import Monoid._
  def sum[A](xs: List[A])(implicit m:Monoid[A]): A = {
    xs.foldLeft(m.mzero)(m.mappend)
  }

  implicit val multiMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a * b
    def mzero: Int = 1
  }

  println(sum(List(1,2,3,4)))

}

