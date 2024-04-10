package util
import Optionals.Optional.*
import util.Optionals.Optional

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

    override def toString: String = this match
      case Cons(h, t) => s"$h :: $t"
      case Nil() => "nil"

  object Sequence:
    def apply[A](elements: A*): Sequence[A] =
      var sequence: Sequence[A] = Nil()
      for e <- elements do
        sequence = Cons(e, sequence)
      sequence.reverse()


    def empty[A]: Sequence[A] = Nil()

    extension [A](sequence: Sequence[A])
      def head: Optional[A] = sequence match
        case Cons(h, _) => Just(h)
        case _ => Empty()

      def concat(other: Sequence[A]): Sequence[A] = sequence match
        case Cons(h, t) => Cons(h, t.concat(other))
        case _ => other

      def flatMap[B](f: A => Sequence[B]): Sequence[B] = sequence match
        case Cons(h, t) => f(h).concat(t.flatMap(f))
        case _ => Nil()

      def map[B](f: A => B): Sequence[B] = sequence.flatMap(x => Cons(f(x), Nil()))

      def filter(f: A => Boolean): Sequence[A] = sequence.flatMap:
        case x if f(x) => Cons(x, Nil())
        case _ => Nil()

      def find(f: A => Boolean): Optional[A] = sequence match
        case Cons(h, t) if f(h) => Just(h)
        case Cons(_, t) => t.find(f)
        case _ => Empty()

      def contains(e: A): Boolean = !sequence.find(_ == e).isEmpty

      def reverse(): Sequence[A] = sequence match
        case Cons(h, t) => t.reverse().concat(Cons(h, Nil()))
        case _ => Nil()

      def anyMatch(p: A => Boolean): Boolean = sequence match
        case Cons(h, Nil()) => p(h)
        case Cons(h, t) => p(h) && t.anyMatch(p)
        case _ => false

      def foldLeft[B](acc: B)(f: (B, A) => B): B = sequence match
        case Cons(h, t) => t.foldLeft(f(acc, h))(f)
        case _ => acc

      def distinct(): Sequence[A] =
        sequence.foldLeft(Sequence.empty[A]) { (acc, curr) =>
          if !acc.contains(curr) then Cons(curr, acc)
          else acc
        }.reverse()


    object sameTag:
      import ex.Item

      def unapply(s: Sequence[Item]): Option[String] =
        s.flatMap(_.tags)
          .distinct()
          .filter(t => s.anyMatch(_.tags.contains(t)))
          .head
        match
          case Just(t) => Option(t)
          case _ => Option.empty

@main def trySequences(): Unit =
  import Sequences.*
  import Sequences.Sequence.sameTag
  import ex.Item

  val sequence = Sequence(1, 2, 3)
  println(sequence)
  println(sequence.head)
  println(sequence.map(_ * 2))
  println(sequence.flatMap(x => Sequence(x, x * 2)))
  println(sequence.filter(_ % 2 == 0))
  println(sequence.concat(Sequence(4, 5, 6)))
  println(sequence.find(_ % 2 == 0))
  println(sequence.contains(2))

  val items = Sequence(
    Item(1, "a", "1", "2"),
    Item(2, "b", "1"),
    Item(3, "c", "2", "1")
  )

  items match
    case sameTag(t) => println(s"$items have same tag $t")
    case _ => println(s"$items have different tags")


