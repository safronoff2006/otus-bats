package homework_tests.Monad


import me.chuwy.otusbats.Monad
import me.chuwy.otusbats.Monad._
import me.chuwy.otusbats.Monad.Tuple2Instans._

import org.scalatest.flatspec.AnyFlatSpec

class MonadTest extends AnyFlatSpec {

  "Monad[F[_]] инстансы " should "должены работать" in {
    assert(Monad[Option].point(4) === Some(4))
    assert(Monad[Option].point("Привет") === Some("Привет"))
    assert(Monad[List].point(true) === List(true))
    assert(Monad[List].map(List(1, 2, 3))(_ * 2) === List(2, 4, 6))
    assert(
      Monad[List].flatMap(List("Панки", "Рокеры"))(x => List(x, "-", if (x.startsWith("П")) "неформалы" else "мотоциклисты"))
        ===
        List("Панки", "-", "неформалы", "Рокеры", "-", "мотоциклисты"))
    assert(Monad[Option].flatten(Some(Some("Значение"))) == Some("Значение"))
    assert(Monad[List].flatten(List(List(1, 2, 3))) === List(1, 2, 3))

  }

  "Monad[F[_]] инстансы  использующие typelevel kind-projector плагин " should "должены работать" in {
    assert(Monad[Either[String, +*]].point(12) === Right(12))
    assert(Monad[Either[String, +*]].map(Right(5))(_ * 5) === Right(25))
    assert(Monad[Either[String, +*]].map(Left("Ошибка"))("") === Left("Ошибка"))
    assert(Monad[Tuple2[*, String]].point(12) === 12 -> "")
    assert(Monad[Tuple2[*, String]].map((12, "Число"))(_ * 2) === 24 -> "Число")
    assert(Monad[Tuple2[*, String]].flatMap(12 -> "хорошо")(x => x -> "плохо") === 12 -> "плохо")
    assert(Monad[Tuple2[*, String]].flatMap(12 -> "хорошо")(x => x * 2 -> "удвоение") === 24 -> "удвоение")
    assert(Monad[Tuple2[Int, *]].map((12, "Число"))(x => x + " " + x) === 12 -> "Число Число")
    assert(Monad[Tuple2[Int, *]].flatMap(5 -> "хорошо")(x => 4 -> (x + " но не очень")) === 4 -> "хорошо но не очень")
    assert(Monad[Tuple2[Int, *]].flatMap(12 -> "12")(x => 24 -> (x + " * 2")) === 24 -> "12 * 2")
    assert(Monad[Tuple2[Boolean, *]].point(24) === false -> 24)

    val fun1: Int => Int = Monad[Function1[Int, *]].point(10)
    assert(fun1(5) === 10)

    val fun2: Int => String = Monad[Function1[Int, *]].map(x => (x * 2).toString)(_ + " удвоение")
    assert(fun2(10) === "20 удвоение")

     val fun3: Int => String =
       Monad[Function1[Int, *]]
         .flatMap(x => x.toString)(z => y => z + " " + (y + 1).toString + " инкремент")

     assert(fun3(10) === "10 11 инкремент")
  }

  "классы расширения синтаксиса " should "должены работать" in {
    assert(4.point[Option] === Some(4))
    assert("Привет".point[Option] === Some("Привет"))
    assert(false.point[List] === List(false))
    assert(35.point[Option].point[Option] === Some(Some(35)))

    val moo = Monad[Option]
    assert(moo.point(24).point[Option].flatten === Some(24))



  }


}
