package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}


object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)
  def from[A](f: A => String): Show[A] = v => f(v)

  implicit  val   showString = from[String](identity)

  implicit  val   showInt: Show[Int] = from[Int](_.toString)

  implicit  val   showBoolean = from[Boolean](_.toString)

  def show[A: Show](v: A): String = {
    Show[A].show(v)
  }

  implicit  def  showList[A:Show]: Show[List[A]] = from[List[A]] {
    list=> "(" + list.foldRight("")( (el, acc) =>  Show[A].show(el) + ',' + acc ).dropRight(1) + ")"
  }

  implicit def  showSet[A:Show]: Show[Set[A]] = from[Set[A]] {
    set => "(" + set.foldLeft("")( (acc,el) =>  Show[A].show(el) + "," + acc).dropRight(1) + ")"
  }

  implicit val showListInt = showList[Int]
  implicit val showListString = showList[String]
  implicit val showListBoolean = showList[Boolean]

  implicit val showSetInt = showSet[Int]
  implicit val showSetString = showSet[String]
  implicit val showSetBoolean = showSet[Boolean]


  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = from[List[A]] {
    list: List[A] => list.foldRight("")((el, acc) =>  ev.show(el) + "," +  acc).dropRight(1)
  }


  // 2. Summoner (apply)

  def apply[A](implicit ev: Show[A]): Show[A] = ev


  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }



  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String =
    begin + list.foldRight("")((el, acc) => el.show + separator + acc).dropRight(separator.length) + end


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = a => a.toString
  
  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = from(f)

  def fromFunction_2[A](f: A => String): Show[A] = a => f(a)


}


object TestShow extends App {
  import Show._

  println(show("Привет"))
  println(show(123))
  println(show(true))
  println(show(List(1,2,3,4)))
  println(show(List("1","2","3","4")))
  println(show(Set(true,false)))
  println(mkString_(List(1,2,3), "[", "]", ","))



  val list: List[Int] = Nil
  list.show

  println(fromFunction((x:Int) => (x*2).toString).show(2))
  println(fromFunction_2((x:Int) => (x+5).toString).show(2))
  println(fromJvm.show(5))

}
