# Implicits in Scala

Вам дана базовая реализация комплексных чисел.

```scala
final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber) =
    ComplexNumber(
      (real * other.real) + (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )

  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)

  def ~=(o: ComplexNumber) =
    (real - o.real).abs < 1e-6 && (imaginary - o.imaginary).abs < 1e-6
}

object ComplexNumber
```

Ваша задача добавить функционал к этой реализации с запретом изменять исходный код класса и его объекта-компаньона, вам
так же запрещено наследоваться от этого класса (final), запрещено использовать паттерны из Java (например, декоратор).

1. Добавить операции вычитания и деления (тут можно кидать нужный runtime exception), а также полярную форму для каждого
   инстанса класса `ComplexNumber`.
2. Добавить возможность использовать все методы с числовыми
   типами ([Numeric](https://www.scala-lang.org/api/2.13.10/scala/math/Numeric.html)).
3. Добавить возможность создавать комплексные числа при помощи специального синтаксиса, напоминающего алгебраическое
   представление комплексных чисел `z = a + bi`.

Тесты для этого домашнего задания писать не нужно. 

### Code Style:

Мы последовательно вводим список запрещенных механик, которыми нельзя пользоваться при написании кода, и рекомендаций по
code style. За нарушения мы оставляем за собой право **снижать оценку**.

* Переменные и функции должны иметь осмысленные названия;
* Тест классы именуются `<ClassName>Spec`, где `<ClassName>` - класс к которому пишутся тесты;
* Тест классы находятся в том же пакете, что и класс к которому пишутся тесты (например, класс `Fibonacci` находится в
  пакете `fibonacci` в директории `src/main/scala/fibonacci`, значит его тест класс `FibonacciSpec` должен быть в том же
  пакете в директории `src/test/scala/fibonacci`);
* Каждый тест должен быть в отдельном test suite;
* Использовать java коллекции запрещается (Используйте `Scala` коллекции);
* Использовать `mutable` коллекции запрещается;
* Использовать `var` запрещается;
* Использование `this` запрещается (используйте `self` если требуется);
* Использование `return` запрещается;
* Использование `System.exit` запрещается;
* Касты или проверки на типы с помощью методов из Java вроде `asInstanceOf` запрещаются;
* Использовать `throw` запрещено;
* Использование циклов запрещается (используйте `for comprehension`, `tailRec`);
* Использование не безопасных вызовов разрешено только в тестах (например `.get` у `Option`);
