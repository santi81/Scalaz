
// Sum Function

def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }

sum(List(1,2,3))


// Monoids

// Its a type for which there exists a mappend which produces another type in the same set
// and also a function that produces a zero


object IntMonoid {

  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}



def MonoidicSum(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)

// Now abstracting on the type of Monoid

trait Monoid[A]
{

  def mappend(a: A, b: A): A
  def mzero: A

}

object IntMonoidV2 extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}

def sum[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

sum(List(1, 2, 3, 4), IntMonoidV2)



//The final change we have to take is to make the Monoid implicit so we don’t have to specify it each time.

def sumFinal[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

implicit val intMonoid = IntMonoidV2

sumFinal(List(1,2,3))


def sum[A: Monoid](xs: List[A]): A = {
  val m = implicitly[Monoid[A]]
  xs.foldLeft(m.mzero)(m.mappend)
}