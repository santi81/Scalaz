

//Parametric Polymorphism


def head[A](xs: List[A]): A = xs(0)

head(1 :: 2:: Nil)

head("Santosh" :: "Kumar" :: "Addanki" :: Nil)

case class car(name : String)

head(car("Civic") :: car("Elantra"):: Nil)



//Subtype Polymorphism

def plus[A](a1: A, a2: A): A = ???

trait Plus[A] {
  def plus(a2: A): A
}


def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)


