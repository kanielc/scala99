import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

implicit class RichInt(x: Int) { 
  import LearnTest._
  
  def isPrime = (2 to x / 2).forall(x % _ != 0)
  
  def isCoprimeTo(y: Int) = gcd(x, y) == 1
  
  def totient = (1 to x).filter(_.isCoprimeTo(x)).length
  
  def primeFactors = {
    def pFac(div: Int, num: Int):List[Int] = {
      if (num < 2) // shouldn't happen
	Nil
      else if (num % div == 0 && div.isPrime)
	div :: pFac(div, num / div) 
      else if (div > num / 2)
	List(num)  // must be a prime
      else
	pFac(div + 1, num)
    }
    
    pFac(2, x)
  }
}
     
object LearnTest extends App {
  def last[T](list: List[T]) = {
    list.last
  }

  // Note, this function is just plain undefined if list size is less than 2
  // Proper definition would be to use an Option
  def penultimate[T](list: List[T]): T = list.reverse.tail.head

  def nth[T](x: Int, list: List[T]) = list(x)

  def printResult(ind: Int, res: Any, expected: Any) {
    println("problem: " + ind + " result: " + (res == expected))

    if (res != expected) {
      println("actual: " + res)
      println("expected: " + expected)
    }
  }

  def length[T](list: List[T]) = list.length

  def reverse[T](list: List[T]) = list.reverse

  def isPalindrome[T](list: List[T]): Boolean =
    list.length match {
      case 0 => true
      case 1 => true
      case _ => list(0) == last(list) && isPalindrome(list.slice(1, list.length - 1))
    }

  def flatten[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case (head :: tail) => (head match {
      case tList@(h :: inner_tail) if tList.isInstanceOf[List[T]] => flatten(tList).asInstanceOf[List[T]]
      case i => List(i).asInstanceOf[List[T]]
    }) ::: flatten(tail)
  }

  def compress[T](list: List[T]) = {
    def cprime(last: Any, list: List[T]): List[T] = list match {      
      case Nil => Nil
      case h::tail => if (last == h) cprime(last, tail) else h :: cprime(h, tail)
    }
    
    cprime(None, list)
  }

  def pack[T](list: List[T]) = {
    def pprime(last: Any, currList: List[T], list: List[T]): List[List[T]] = list match {
      case Nil => List(currList)
      case h::tail if (currList.length == 0) => pprime(h, List(h), tail)
      case h::tail => if (h == last) pprime(last, h :: currList, tail) else currList :: pprime(h, List(h), tail)      
    }
    
    pprime(None, Nil, list)
  }

  def encode[T](list: List[T]):List[(Int, T)] = {
    (pack(list)).map((l: List[T]) => (l.length, l(0)))
  }

  def encodeModified[T](list: List[T]) = {
    (pack(list)).map((l: List[T]) => l.length match {
      case 1 => l(0)
      case _ => (l.length, l(0))
    })
  }

  def decode(list: List[Any]) = flatten(list.map(item => item match {
    case (count: Int, b) => (1 to count).map(_ => b).toList
    case b => b
  }))

  // this one is just silly, why would I do it in an inferior way?
  def encodeDirect[T](list: List[T]) = {
    (pack(list)).map((l: List[T]) => (l.length, l(0)))
  }

  def duplicate[T](list: List[T]) = flatten(list.map(x => List(x, x)))

  def duplicateN[T](times: Int, list: List[T]) = flatten(list.map(x => (1 to times).map(_ => x).toList))

  def drop[T](pos: Int, list: List[T]): List[T] = list.view.zipWithIndex.filter(e => if ((e._2 + 1) % pos == 0) false else true).map(_._1).toList

  def split[T](pos: Int, list: List[T]) = list.splitAt(pos)

  def slice[T](start: Int, end: Int, list: List[T]) = list.slice(start, end)

  def rotate[T](pos: Int, list: List[T]) = {
    val start = if (pos >= 0) pos else list.length + pos

    val (endSlice, startSlice) = split(start, list)
    startSlice ::: endSlice
  }

  def insertAt[T](item: T, pos: Int, list: List[T]) = list.slice(0, pos) ::: item :: list.slice(pos, list.length)

  def range(s: Int, e: Int) = (s to e).toList

  def removeAt[T](pos: Int, list: List[T]) = (list.slice(0, pos) ::: list.slice(pos + 1, list.length), list(pos))

  val rand = scala.util.Random

  def randomSelect[T](num: Int, list: List[T]) = rand.shuffle((0 until list.length).toList).take(num).map(list(_))

  def lotto(num: Int, max: Int) = rand.shuffle((1 to max).toList).slice(0, num)

  def randomPermute[T](list: List[T]) = rand.shuffle(list)

  def combinations[T](k: Int, list: List[T]): List[List[T]] =
    list match {
      case Nil => Nil
      case head :: xs =>
        if (k <= 0 || k > list.length) {
          Nil
        } else if (k == 1) {
          list.map(List(_))
        } else {
          combinations(k - 1, xs).map(head :: _) ::: combinations(k, xs)
        }
    }

  def group3[T](list: List[T]): List[List[List[T]]] = group((2, 3, 4), list)

  def group[T](sizes: (Int, Int, Int), list: List[T]): List[List[List[T]]] = {
    val result = new ListBuffer[List[List[T]]]

    val com2 = combinations(sizes._1, list).foreach {
      i =>
        val com3 = combinations(sizes._2, list.filterNot(i.contains(_)))
        com3.foreach {
          item =>
            val filList = list.filterNot(elem => (item.contains(elem) || i.contains(elem)))
            result += List(i, item, filList)
        }
    }
    result.toList
  }

  def lsort[T](list: List[List[T]]) = list.sortBy(_.length)

  def lsortFreq[T](list: List[List[T]]) = {
    def lp[T](list: List[List[T]], m: scala.collection.mutable.Map[Int, Int]): scala.collection.mutable.Map[Int, Int] = {
      list.foreach(e => m(e.length) += 1)
      m
    }

    val valMap = lp(list, scala.collection.mutable.Map[Int, Int]().withDefaultValue(0))
    list.sortBy(e => valMap(e.length))
  }
  
  def gcd(x: Int, y: Int):Int = {
    if (x == y) 
      x
    else if (x > y)
      gcd(y, x)
    else
      gcd(x, y - x)   
  }
      

  var res: Any = last(List(1, 1, 2, 3, 5, 8))
  printResult(1, res, 8)

  res = penultimate(List(1, 1, 2, 3, 5, 8))
  printResult(2, res, 5)

  res = nth(2, List(1, 1, 2, 3, 5, 8))
  printResult(3, res, 2)

  res = length(List(1, 1, 2, 3, 5, 8))
  printResult(4, res, 6)

  res = reverse(List(1, 1, 2, 3, 5, 8))
  printResult(5, res, List(8, 5, 3, 2, 1, 1))

  res = isPalindrome(List(1, 2, 3, 2, 1))
  printResult(6, res, true)

  res = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  printResult(7, res, List(1, 1, 2, 3, 5, 8))

  res = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  printResult(8, res, List('a, 'b, 'c, 'a, 'd, 'e))

  res = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  printResult(9, res, List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))

  res = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  printResult(10, res, List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))

  res = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  printResult(11, res, List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))

  res = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  printResult(12, res, List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

  res = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  printResult(13, res, List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))

  res = duplicate(List('a, 'b, 'c, 'c, 'd))
  printResult(14, res, List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))

  res = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  printResult(15, res, List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))

  res = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  printResult(16, res, List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))

  res = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  printResult(17, res, (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  res = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  printResult(18, res, List('d, 'e, 'f, 'g))

  res = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  printResult(19, res, List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

  res = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  printResult(19, res, List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

  res = removeAt(1, List('a, 'b, 'c, 'd))
  printResult(19, res, (List('a, 'c, 'd), 'b))

  res = lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  printResult(28, res, List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))

  res = lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  printResult(28, res, List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
  
  res = 7.isPrime
  printResult(31, res, true)
  
  res = gcd(36, 63)
  printResult(32, res, 9)
  
  res = 35.isCoprimeTo(64)
  printResult(33, res, true)
  
  res = 10.totient
  printResult(34, res, 4)
  
  res = 315.primeFactors
  printResult(35, res, List(3, 3, 5, 7))
}

LearnTest.main(null)