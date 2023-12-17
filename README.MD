# Реализация функционала заданий  к уроку Сats core, data types and type classes ( урок №13)
## Задание реализации тайп класса Show - реализовано:
### Трейт Show
### Объект - компаньон
### Смарт - конструктор from - для менее многословного создания имплиситных инстансов
### Инстансы для простых типов - имплисит значения
*  implicit  val   showString 
*  implicit  val   showInt
*  implicit  val   showBoolean
### Инстансы тайп-конструкторов (контейнеров) - имплисит преобразования - использутся Context Bound Implicit (суммонер) и смарт-конструктор
* implicit  def  showList
* implicit  def  showSet
### Суммонер def apply[A](implicit ev: Show[A]): Show[A]
### Обобщенные функции
* implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] - реализована with conditional implicit, как требуется в задании
### имплиситный класс расширения синтаксиса ShowOps[A](a: A) 
* для класса реализван метод show (используется суммонер)
* создана функция mkString_ - используется суммонер
### Созданы Helper constructors
* fromJvm
* fromFunction
* fromFunction_2
## Задание реализации тайп класса Monad - реализовано:
### Трейты
* FlapMap - трейт создан для более выразительной реализации инстансов монады Tuple2
* Monad - наследует FlatMap и Functor
### Суммонер
### Инстансы (имплисит значения) для тайп-конструкторов параметризуемых одним параметром
* monadOption
* monadList
### Инстансы (имплисит преобразования) для тайп-конструкторов параметризуемых двумя параметрами с использованием плагина typelevel kind-projector
* monadEitherTypedLeft[E]
* monadFunction1[T]
### Группа инстансов для типа Tuple2 - включает
* Инстансы трейта FlatMap (через которые потом имплиситно выражаются инстансы монады ): flatMapTuple2_1[T], flatMapTuple2_2[T] 
* Инстансы монады имплиситно использующие инстансы трейта FlatMap:  monadTuple2_1Int, monadTuple2_1String, monadTuple2_1Boolean, monadTuple2_2Int, monadTuple2_2String, monadTuple2_2Boolean 
### Имплисит классы расширения синтаксиса
* MonadFromA
* MonadFromFA
* MonadFromFFA

## Тесты 
### Unit-тесты тайп-класса Show  Путь: src/test/scala/homework_tests/Show/Show.scala
### Unit-тесты тайп-класса Monad Путь: src/test/scala/homework_tests/Monad/Monad.scala
* Тест Monad[F[_]] ординарно реализуемых инстансов 
* Тест Monad[F[_]] инстансов  использующиx typelevel kind-projector плагин
* Тест имплисит-классов расширения синтаксиса



