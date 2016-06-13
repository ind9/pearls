package pearls.naive

trait SemiRing[T] {
  def zero:T
  def one:T
  def sum(a:T, b:T):T
  def prod(a:T, b:T):T
}

sealed trait Regex[T] {
  def splits(str: String) = {
    0.to(str.length).map {f =>
      (str.substring(0,f), str.substring(f))
    }
  }
  def matches(str:String)(implicit ops:SemiRing[T]):T

}

case class Epsilon[T]() extends Regex[T] {
  override def matches(str: String)(implicit ops:SemiRing[T]) = if(str.isEmpty) ops.one else ops.zero
}
case class Sym[T](char:Char => T) extends Regex[T] {
  override def matches(str: String)(implicit ops:SemiRing[T]): T = {
    if(str.length == 1) char(str.charAt(0)) else ops.zero
  }

}
object Sym {
  def apply[T](char:Char)(implicit ops:SemiRing[T]):Sym[T] = Sym ({ x:Char =>
    if(x == char) ops.one else ops.zero
  })
}

case class Alt[T](first:Regex[T], other:Regex[T]) extends Regex[T] {
  override def matches(str: String)(implicit ops:SemiRing[T]) = ops.sum(first.matches(str), other.matches(str))
}
case class Seq[T](first:Regex[T], follow:Regex[T]) extends Regex[T] {
  override def matches(str: String)(implicit ops:SemiRing[T]) = {
    splits(str).map {
      case (f, s) => ops.prod(first.matches(f), follow.matches(s))
    }.foldLeft(ops.zero)(ops.sum)
  }
}
case class Rep[T](re:Regex[T]) extends Regex[T] {
  override def matches(str: String)(implicit  ops:SemiRing[T]): T = {
    if(str.isEmpty) ops.one
    else splits(str).tail.map {
      case (f,s) => ops.prod(re.matches(f), this.matches(s))
    }.foldLeft(ops.zero)(ops.sum)
  }
}

object SemiRing {
  implicit val boolOps = new SemiRing[Boolean] {
    override def zero: Boolean = false
    override def one: Boolean = true
    override def prod(a: Boolean, b: Boolean): Boolean = a && b
    override def sum(a: Boolean, b: Boolean): Boolean = a || b
  }

  implicit val wholeNumberOps = new SemiRing[Int] {
    override def zero: Int = 0
    override def one: Int = 1
    override def prod(a: Int, b: Int): Int = a * b
    override def sum(a: Int, b: Int): Int = a + b
  }
}


object Main {
  def main(args:Array[String]): Unit = {
    import SemiRing.wholeNumberOps
    val re =
      Seq(
        Rep(
          Seq(
            Seq(
              Seq(
                Rep(
                  Alt(Sym('a'), Sym('b'))),
                Sym('c')),
              Rep(
                Alt(Sym('a'), Sym('b')))),
            Sym('c'))),
        Rep(
          Alt(Sym('a'), Sym('b'))))

    val re2 = Seq(Rep(Alt(Sym('a'),Sym('b'))), Sym('c'))
    val re3 = Rep(Seq(re2, re2))
    val fin = Seq(re3, Rep(Alt(Sym('a'),Sym('b'))))

    List(
      "cc",
      "abccaaa",
      "acccccc",
      "acaca",
      "acacac"
    ).foreach { str =>
      println(str, fin.matches(str))

    }

//    println(Alt(sym('b'), Rep(sym('b'))).matches("b"))
  }
}