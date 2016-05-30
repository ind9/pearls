package other

sealed trait RegexBool {
  def matches(str:String) = {
    str.tail.foldLeft(this.shift(str.head, true)){
      (re, char) => re.shift(char, false)
    }.isFinal
  }
  def shift(char:Char, last:Boolean):RegexBool
  def acceptEmpty:Boolean
  def isFinal:Boolean
}
case class Epsilon() extends RegexBool {
  override def shift(char: Char, last: Boolean): RegexBool = this
  override def acceptEmpty: Boolean = true
  override def isFinal: Boolean = false
}

case class Sym(char:Char, mark:Boolean = false) extends RegexBool {
  override def shift(current: Char, last: Boolean): RegexBool =
    if(char == current) Sym(char, last) else Sym(char, false)
  override def acceptEmpty: Boolean = false
  override def isFinal: Boolean = mark
}

case class Alt(first:RegexBool, other:RegexBool) extends RegexBool {
  override def shift(char: Char, last: Boolean): RegexBool = {
    Alt(first.shift(char, last), other.shift(char, last))
  }
  override def acceptEmpty: Boolean = first.acceptEmpty || other.acceptEmpty
  override def isFinal: Boolean = first.isFinal || other.isFinal
}

case class Seq(first:RegexBool, other:RegexBool) extends RegexBool {
  override def shift(char: Char, last: Boolean): RegexBool = {
    Seq(first.shift(char, last), other.shift(char, (last && first.acceptEmpty) || first.isFinal))
  }
  override def acceptEmpty: Boolean = first.acceptEmpty && other.acceptEmpty
  override def isFinal: Boolean = (first.isFinal && other.acceptEmpty) || other.isFinal
}

case class Rep(re:RegexBool) extends RegexBool {
  override def shift(char: Char, last: Boolean): RegexBool = Rep(re.shift(char, last || re.isFinal))
  override def isFinal: Boolean = re.isFinal
  override def acceptEmpty: Boolean = true
}

object Main {
  def main(args:Array[String]) = {
    val re: Seq =
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
  }
}
