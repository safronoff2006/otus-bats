package homework_tests.Show

import org.scalatest.flatspec.AnyFlatSpec

class ShowTest extends AnyFlatSpec {
  import me.chuwy.otusbats.Show._

  "show" should "должен соответствовать ожидаемому поведению" in {
    assert(show("Привет") === "Привет")
    assert(show(123) === "123")
    assert(show(true) === "true")
    assert(show(show(List(1,2,3,4))) === "(1,2,3,4)")
    assert(show(Set(true,false)) === "(false,true)")
    assert(mkString_(List(1,2,3), "[", "]", ",") ===  "[1,2,3]")

  }

  "fromFunction" should "должен соответствовать ожидаемому поведению" in {
    assert(fromFunction((x:Int) => (x*2).toString).show(2) === "4" )
    assert(fromFunction_2((x:Int) => (x+5).toString).show(2) === "7")
  }

  "fromJvm" should "должен соответствовать ожидаемому поведению" in {
    assert(fromJvm.show(5) === "5")
  }


}
