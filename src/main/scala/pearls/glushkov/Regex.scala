package pearls.glushkov

import pearls.naive.SemiRing

sealed trait Regex[C,T] {
  def matches(str:Iterable[C])(implicit ops:SemiRing[T]) = {
    str.tail.foldLeft(this.shift(str.head, ops.one)){
      (re, char) => re.shift(char, ops.zero)
    }.finall
  }

  def substring(str:Iterable[C])(implicit ops:SemiRing[T]) = {
    Seq(Rep(Sym.arb[C,T]), Seq(this, Rep(Sym.arb[C,T]))).matches(str)
  }
  def shift(char:C, last:T)(implicit ops:SemiRing[T]):Regex[C,T]
  def acceptEmpty(implicit ops:SemiRing[T]):T
  def finall(implicit ops:SemiRing[T]):T
}
case class Epsilon[C,T]() extends Regex[C,T] {
  override def shift(char: C, last: T)(implicit ops:SemiRing[T]): Regex[C,T] = this
  override def acceptEmpty(implicit ops:SemiRing[T]): T = ops.one
  override def finall(implicit ops:SemiRing[T]): T = ops.zero
}

case class Sym[C,T](char:C => T) extends Regex[C,T] {
  override def shift(current: C, last: T)(implicit ops:SemiRing[T]): Regex[C,T] =
    new Sym(char) {
      override def finall(implicit ops:SemiRing[T]) = ops.prod(char(current),last)
    }
  override def acceptEmpty(implicit ops:SemiRing[T]): T = ops.zero
  override def finall(implicit ops:SemiRing[T]): T = ops.zero
}

object Plus {
  def apply[C,T](regex:Regex[C,T]) = Seq(regex, Rep(regex))
}

object Sym {
  def apply[C,T](char:C)(implicit ops:SemiRing[T]):Sym[C,T] = Sym(c => {if(c == char) ops.one else ops.zero})
  def arb[C,T](implicit ops:SemiRing[T]) = Sym[C,T]({c:C => ops.one})
}

case class Alt[C,T](first:Regex[C,T], other:Regex[C,T]) extends Regex[C,T] {
  override def shift(char: C, last: T)(implicit ops:SemiRing[T]): Regex[C,T] = {
    Alt(first.shift(char, last), other.shift(char, last))
  }
  override def acceptEmpty(implicit ops:SemiRing[T]): T = ops.sum(first.acceptEmpty, other.acceptEmpty)
  override def finall(implicit ops:SemiRing[T]) = ops.sum(first.finall, other.finall)
}

case class Seq[C,T](first:Regex[C,T], other:Regex[C,T]) extends Regex[C,T] {
  override def shift(char: C, last: T)(implicit ops:SemiRing[T]): Regex[C,T] = {
    Seq(first.shift(char, last), other.shift(char, ops.sum(ops.prod(last, first.acceptEmpty) ,first.finall)))
  }
  override def acceptEmpty(implicit ops:SemiRing[T]): T = ops.prod(first.acceptEmpty, other.acceptEmpty)
  override def finall(implicit ops:SemiRing[T]): T = ops.sum(ops.prod(first.finall , other.acceptEmpty),other.finall)
}

case class Rep[C,T](re:Regex[C,T]) extends Regex[C,T] {
  override def shift(char: C, last: T)(implicit ops:SemiRing[T]): Regex[C,T] =
    Rep(re.shift(char, ops.sum(last ,re.finall)))
  override def finall(implicit ops:SemiRing[T]): T = re.finall
  override def acceptEmpty(implicit ops:SemiRing[T]): T = ops.one
}

object Main {
  def main(args:Array[String]) = {
    import SemiRing.boolOps
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


    println(re2.matches("aac"))
    List(
      "cc",
      "abccaaa",
      "acccccc",
      "acaca",
      "acacac"
    ).foreach { str =>
      println(str, fin.matches(str))
    }

    def anbn = Alt(Epsilon, Seq(Sym('a'), Seq(anbn, Sym('b'))))

    println("Is substring match" + Seq(Sym('a'),Sym('b')).substring("ababbab"))
    print(Alt(Sym('b'), Rep(Sym('b'))).matches("b"))
  }
}
