trait tSet

trait PureSet extends tSet {
  def map(f: PureSet => PureSet): PureSet

  def isEmpty: Boolean
  def size: Int
  def cardinality: Int
  def members: List[PureSet]
  def is(s: PureSet): Boolean
  def hasMember(el: PureSet): Boolean
  def hasSubset(s: PureSet): Boolean
  def isSubsetOf(s: PureSet): Boolean
  def powerSet: PureSet
  def unionWith(s: PureSet): PureSet
  def union: PureSet
  def intersectionWith(s: PureSet): PureSet
  def intersection: PureSet
  def relativeComplementIn(b: PureSet): PureSet

  def reachIn(i: Int): PureSet  //Order doesn't matter, so this operation is theoretically non-deterministic

  def listMem(): Unit
}

object PureSet {
  def apply(): PureSet = new EmptyPureSet
  def apply(l: List[PureSet]): PureSet = new NonEmptyPureSet(l)

  def pair(s: PureSet, t: PureSet): PureSet = PureSet(List(s, t))
  def unit(s: PureSet): PureSet = pair(s, s)

  def oPair(s: PureSet, t: PureSet): PureSet = PureSet(List(s, PureSet(List(s, t))))
  def oTuple(l: List[PureSet]): PureSet = l.reduceLeft((s, t) => {
    PureSet.oPair(s, t)
  })  //But how can we unwind this, given that the first set in the tuple might have a size > 1?
  def fst(p: PureSet): PureSet = {
    if (p.size != 2) throw new Exception("You can only take fst on a set with two elements")
    val s = p.reachIn(0)
    val t = p.reachIn(1)
    if (s.isSubsetOf(t)) s else t //This assumes we have an oPair
  }
  def snd(p: PureSet): PureSet = {
    if (p.size != 2) throw new Exception("You can only take snd on a set with two elements")
    val s = p.reachIn(0)
    val t = p.reachIn(1)
    if (s.isSubsetOf(t)) t else s //This assumes we have an oPair
  }

  def deduplicate(l: List[PureSet]) = l.foldLeft(List[PureSet]())((acc: List[PureSet], x: PureSet) => {
    if (acc.exists((s: PureSet) => s.is(x))) acc else acc :+ x
  })

  def withMember(s: PureSet) = unit(s)
}

case class EmptyPureSet extends PureSet {
  def map(f: PureSet => PureSet): PureSet = this

  def isEmpty: Boolean = true
  def size: Int = 0
  def cardinality: Int = size
  def members: List[PureSet] = Nil
  def is(s: PureSet): Boolean = s.isEmpty
  def hasMember(el: PureSet): Boolean = false
  def hasSubset(s: PureSet): Boolean = false
  def isSubsetOf(s: PureSet): Boolean = true
  def powerSet: PureSet = PureSet.unit(this)
  def unionWith(s: PureSet): PureSet = s
  def union: PureSet = this
  def intersectionWith(s: PureSet): PureSet = this
  def intersection: PureSet = this
  def relativeComplementIn(b: PureSet): PureSet = this

  def reachIn(i: Int): PureSet = this

  override def toString: String = "0"
  def listMem(): Unit = println("")
}

case class NonEmptyPureSet(l: List[PureSet]) extends PureSet {
  val m = PureSet.deduplicate(l)

  def map(f: PureSet => PureSet): PureSet = PureSet(m.map(f))

  def isEmpty: Boolean = false
  def size: Int = m.size
  def cardinality: Int = size
  def members: List[PureSet] = m
  def is(s: PureSet): Boolean = m.forall(s.hasMember _) && s.size == size
  def hasMember(el: PureSet): Boolean = m.exists((mem: PureSet) => mem.is(el))
  def hasSubset(s: PureSet): Boolean = s.isSubsetOf(this)
  def isSubsetOf(s: PureSet): Boolean = m.forall(s.hasMember _)
  def powerSet: PureSet = pureSetCombinations(m)
  def unionWith(s: PureSet): PureSet = PureSet(m ++ s.members)
  def union: PureSet = m.foldLeft(PureSet())((acc: PureSet, x: PureSet) => acc.unionWith(x))
  def intersectionWith(s: PureSet): PureSet = {
    m.foldLeft(PureSet())((acc: PureSet, x: PureSet) => x match {
      case a if s.hasMember(a) => acc.unionWith(a)
      case _ => acc
    })
  }
  def intersection: PureSet = m.foldLeft(PureSet())((acc: PureSet, x: PureSet) => acc.intersectionWith(x))
  def relativeComplementIn(b: PureSet): PureSet = {
    m.foldLeft(PureSet())((acc: PureSet, x: PureSet) => x match {
      case a if !b.hasMember(a) => acc.unionWith(a)
      case _ => acc
    })
  }

  def reachIn(i: Int): PureSet = m(i) //Order doesn't matter, so this operation is theoretically non-deterministic

  override def toString: String = "{" + membersToString + "}"
  def listMem(): Unit = for (el <- m) println(el)

  private def membersToString: String = m.foldLeft("")((acc, x) => acc + x.toString + ", ").dropRight(2)
}



def pureSetCombinations(l: List[PureSet]): PureSet = {
  def prefix(el: PureSet, ss: List[PureSet]): List[PureSet] = {
    ss.map(PureSet.unit _).map(_.unionWith(PureSet.unit(el))) :+ el
  }

  def naryGroups(lst: List[PureSet], n: Int): List[PureSet] = {
    def loop(l: List[PureSet], acc: List[PureSet]): List[PureSet] = {
      l match {
        case Nil => acc
        case hd :: tl if n == 1 => prefix(hd, tl)
        case h :: t => prefix(h, naryGroups(t, n - 1)) ++ naryGroups(t, n)
      }
    }

    loop(lst, Nil)
  }

  def combinations(ms: List[PureSet]): PureSet = {
    def loop(l: List[PureSet], acc: List[PureSet], k: Int): List[PureSet] = {
      k match {
        case x if x > ms.size => acc
        case 0 => loop(ms, acc :+ PureSet(), 1)
        case 1 => {
          val singles = ms ++ ms.map(PureSet.unit _)
          loop(ms, acc ++ singles, 2)
        }
        case n => loop(ms, acc ++ naryGroups(ms, n), k + 1)
      }
    }

    val members = loop(ms, Nil, 0)

    PureSet(members)
  }

  combinations(l)
}



//ASSERTIONS

val p0 = PureSet()
val p1 = PureSet().powerSet
val p2 = PureSet().powerSet.powerSet
val p3 = PureSet().powerSet.powerSet.powerSet
val p4 = PureSet().powerSet.powerSet.powerSet.powerSet
//val p5 = PureSet().powerSet.powerSet.powerSet.powerSet.powerSet

def test = {
  assert(p0.size == 0)
  assert(p1.size == Math.pow(2, 0))
  assert(p2.size == Math.pow(2, 1))
  assert(p3.size == Math.pow(2, 2))
  assert(p4.size == Math.pow(2, 4))
//  assert(p5.size == Math.pow(2, 16))
}
