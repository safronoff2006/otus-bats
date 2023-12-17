package me.chuwy.otusbats


trait Monad[F[_]] extends FlatMap[F] {  //трейт Monad выражаем через трейты FlatMap и Functor
  self =>
  def point[A](a: A): F[A]
}


trait FlatMap[F[_]] extends Functor[F] {
  self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)

}


object Monad {

  //суммонер
  def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev

  //инстансы
  implicit val monadOption: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)


  }


  implicit val monadList: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)


  }



  //используем typelevel kind-projector плагин


  implicit def monadEitherTypedLeft[E] = new Monad[Either[E, +*]] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)

    override def point[A](a: A): Either[E, A] = Right(a)

    override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.map(f)


  }

  implicit def monadFunction1[T] = new Monad[Function1[T, *]] {
    override def point[A](a: A): T => A = _ => a

    override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B = t => f(fa(t))(t)

    override def map[A, B](fa: T => A)(f: A => B): T => B = t => f(fa(t))
  }


  object Tuple2Instans {    // инстансы MonadTuple2  из  FlatMapTuple2

    implicit def flatMapTuple2_1[T] = new FlatMap[Tuple2[*, T]] {

      override def flatMap[A, B](fa: (A, T))(f: A => (B, T)): (B, T) = f(fa._1)

      override def map[A, B](fa: (A, T))(f: A => B): (B, T) = f(fa._1) -> fa._2

    }

    implicit def flatMapTuple2_2[T] = new FlatMap[Tuple2[T, *]] {

      override def flatMap[A, B](fa: (T, A))(f: A => (T, B)): (T, B) = f(fa._2)

      override def map[A, B](fa: (T, A))(f: A => B): (T, B) = fa._1 -> f(fa._2)


    }

    implicit def monadTuple2_1Int(implicit fm: FlatMap[Tuple2[*, Int]]): Monad[Tuple2[*, Int]] = new Monad[Tuple2[*, Int]] {
      override def point[A](a: A): (A, Int) = a -> 0

      override def flatMap[A, B](fa: (A, Int))(f: A => (B, Int)) = fm.flatMap(fa)(f)

      override def map[A, B](fa: (A, Int))(f: A => B) = fm.map(fa)(f)
    }

    implicit def monadTuple2_1String(implicit fm: FlatMap[Tuple2[*, String]]): Monad[Tuple2[*, String]] = new Monad[Tuple2[*, String]] {
      override def point[A](a: A): (A, String) = a -> ""

      override def flatMap[A, B](fa: (A, String))(f: A => (B, String)) = fm.flatMap(fa)(f)

      override def map[A, B](fa: (A, String))(f: A => B) = fm.map(fa)(f)
    }

    implicit def monadTuple2_1Boolean(implicit fm: FlatMap[Tuple2[*, Boolean]]): Monad[Tuple2[*, Boolean]] = new Monad[Tuple2[*, Boolean]] {
      override def point[A](a: A): (A, Boolean) = a -> false

      override def flatMap[A, B](fa: (A, Boolean))(f: A => (B, Boolean)): (B, Boolean) = fm.flatMap(fa)(f)

      override def map[A, B](fa: (A, Boolean))(f: A => B): (B, Boolean) = fm.map(fa)(f)
    }

    implicit def monadTuple2_2Int(implicit fm: FlatMap[Tuple2[Int, *]]): Monad[Tuple2[Int, *]] = new Monad[Tuple2[Int, *]] {
      override def point[A](a: A): (Int, A) = 0 -> a

      override def flatMap[A, B](fa: (Int, A))(f: A => (Int, B)) = fm.flatMap(fa)(f)

      override def map[A, B](fa: (Int, A))(f: A => B) = fm.map(fa)(f)
    }

    implicit def monadTuple2_2String(implicit fm: FlatMap[Tuple2[String, *]]): Monad[Tuple2[String, *]] = new Monad[Tuple2[String, *]] {
      override def point[A](a: A): (String, A) = "" -> a

      override def flatMap[A, B](fa: (String, A))(f: A => (String, B)) = fm.flatMap(fa)(f)

      override def map[A, B](fa: (String, A))(f: A => B) = fm.map(fa)(f)
    }

    implicit def monadTuple2_2Boolean(implicit fm: FlatMap[Tuple2[Boolean, *]]): Monad[Tuple2[Boolean, *]] = new Monad[Tuple2[Boolean, *]] {
      override def point[A](a: A): (Boolean, A) = false -> a

      override def flatMap[A, B](fa: (Boolean, A))(f: A => (Boolean, B)) = fm.flatMap(fa)(f)

      override def map[A, B](fa: (Boolean, A))(f: A => B) = fm.map(fa)(f)
    }
  }





  //классы расширения синтаксиса

  implicit class MonadFromA[A](a: A) {
    def point[F[_] : Monad]: F[A] = Monad[F].point(a)
  }


  implicit class MonadFromFA[F[_] : Monad, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = Monad[F].map(fa)(f)

    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }

  implicit class MonadFromFFA[F[_] : Monad, A](ffa: F[F[A]]) {
    def flatten: F[A] = Monad[F].flatten(ffa)
  }


}

object TestMonad extends App {

  import Monad._
  import Tuple2Instans._

  val oInt: Option[Int] = Monad[Option].point(4)
  println(oInt)

  val oString: Option[String] = Monad[Option].point("Привет")
  println(oString)

  val oInt2: Option[Int] = 4.point[Option]
  println(oInt2)

  val oString2: Option[String] = "Привет".point[Option]
  println(oString2)

  val lBool: List[Boolean] = Monad[List].point(true)
  println(lBool)

  val lBool2: List[Boolean] = false.point[List]
  println(lBool2)

  val lInt1: List[Int] = Monad[List].map(List(1, 2, 3))(_ * 2)
  println(lInt1)

  val sList1: List[String] = Monad[List].flatMap(List("Панки", "Рокеры"))(x => List(x, "-", if (x.startsWith("П")) "неформалы" else "мотоциклисты"))
  println(sList1)

  val some = Monad[Option].flatten(Some(Some("Значение")))
  println(some)

  val list = Monad[List].flatten(List(List(1, 2, 3)))
  println(list)

  val eith1: Either[String, Int] = Monad[Either[String, +*]].point(12)
  println(eith1)

  val eith2: Either[String, Int] = Monad[Either[String, +*]].map(Right(5))(_ * 5)
  println(eith2)

  val eith3: Either[String, Char] = Monad[Either[String, +*]].map(Left("Ошибка"))("")
  println(eith3)

  val tupl0: (Int, String) = Monad[Tuple2[*, String]].point(12)
  println(tupl0)

  val tupl1: (Int, String) = Monad[Tuple2[*, String]].map((12, "Число"))(_ * 2)
  println(tupl1)


  val tupl2 = Monad[Tuple2[*, String]].flatMap(12 -> "хорошо")(x => x -> "плохо")
  println(tupl2)


  val tupl3 = Monad[Tuple2[*, String]].flatMap(12 -> "хорошо")(x => x * 2 -> "удвоение")
  println(tupl3)


  val tupl4 = Monad[Tuple2[Int, *]].map((12, "Число"))(x => x + " " + x)
  println(tupl4)


  val tupl5 = Monad[Tuple2[Int, *]].flatMap(5 -> "хорошо")(x => 4 -> (x + " но не очень"))
  println(tupl5)

  val tupl6 = Monad[Tuple2[Int, *]].flatMap(12 -> "12")(x => 24 -> (x + " * 2"))
  println(tupl6)

  val tupl7 = Monad[Tuple2[Boolean, *]].point(24)
  println(tupl7)

  val optopt: Option[Option[Int]] = 35.point[Option].point[Option]
  println(optopt)

  val moo = Monad[Option]
  val opt = moo.point(24).point[Option].flatten
  println(opt)

  val fun1: Int => Int = Monad[Function1[Int, *]].point(10)
  println(fun1(5))

  val fun2: Int => String = Monad[Function1[Int, *]].map(x => (x*2).toString)(_ + " удвоение")
  println(fun2(10))


  val fun3 = Monad[Function1[Int, *]].flatMap(x => x.toString)(z => y => z + " " + (y + 1).toString + " инкремент")
  println(fun3(10))
}

