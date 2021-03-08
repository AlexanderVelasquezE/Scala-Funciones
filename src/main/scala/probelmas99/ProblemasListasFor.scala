package probelmas99

import scala.annotation.tailrec
import scala.runtime.Nothing$


//https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
object ProblemasListasFor extends App{

  /*
  Problem 1
  (*) Find the last element of a list.

  Example in Haskell:

  λ> myLast [1,2,3,4]
  4
  λ> myLast ['x','y','z']
  'z'
   */

  //def myLast[A](lst:List[A]):A = lst(lst.length-1)
  //precondicion la lista debe tener al menos un elemento
  def myLast[A](lst:List[A]):A = (for {
      x <- lst
    } yield (_:A) => x).foldLeft(lst.head)((e,f) => f(e))


  //println(myLast(List(1,2,3,4)))
  //println(myLast(List('x','y','z')))

  /*
  Problem 2
  (*) Find the last but one element of a list.

  Example in Haskell:

  λ> myButLast [1,2,3,4]
  3
  λ> myButLast ['a'..'z']
  'y'

   */
  //Precondicion la lista debe tener al menos 2 elementos
  def myButLast[A](lst:List[A]):A = {
    val a = for {
      x <- lst
      if x != lst.last
    } yield x
    val b = for {
      x <- a
      if x == a.last
    } yield x
    b.head
  }
  //println(myButLast(List(1,2,3,4)))
  //println(myButLast(List('x','y','z')))

  /*
  Problem 3
  (*) Find the K'th element of a list. The first element in the list is number 1.

  Example:

  * (element-at '(a b c d e) 3)
  c
  Example in Haskell:

  λ> elementAt [1,2,3] 2
  2
  λ> elementAt "haskell" 5
  'e'
   */
  //precondicion la lista debe tener una cantidad de elementos superior a la posicion indicada
  def elementAt[A](lst:List[A],pos:Int):Option[A] = (for {
      x <- lst
    } yield (t:(Int,Option[A])) => t._2 match {
    case None => if (t._1 == pos) (t._1,Some(x)) else (t._1+1,None)
    case Some(x) => (t._1,Some(x))
  }).foldLeft((0, None:Option[A]))((e, f) => f(e))._2

//  println(elementAt(List(1,2,3),2))
  //println(elementAt(List("H","a","s","k","e","l","l"),5))

  /*
  Problem 4
  (*) Find the number of elements of a list.

  Example in Haskell:

  λ> myLength [123, 456, 789]
  3
  λ> myLength "Hello, world!"
  13
   */

  def myLength[A](lst:List[A]):Int = (for {
    _ <- lst
  } yield (a:Int) => a+1).foldLeft(0)((e,f) => f(e))

  //println(myLength(List(1,2,3,4,5)))
  //println(myLength(List()))

  /*
  Problem 5
  (*) Reverse a list.

  Example in Haskell:

  λ> myReverse "A man, a plan, a canal, panama!"
  "!amanap ,lanac a ,nalp a ,nam A"
  λ> myReverse [1,2,3,4]
  [4,3,2,1]
   */

  def myReverse[A](lst:List[A]):List[A] = (for {
    x <- lst
  } yield ((a:List[A]) => x :: a)).foldLeft(List():List[A])((e,f) => f(e))

  println(myReverse(List(1,2,3,4,5)))

  /*
  Problem 6
  (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

  Example in Haskell:

  λ> isPalindrome [1,2,3]
  False
  λ> isPalindrome "madamimadam"
  True
  λ> isPalindrome [1,2,4,8,16,8,4,2,1]
  True
   */
  def isPalindrome[A](lst:List[A]):Boolean = myReverse(lst)==lst

  //println(isPalindrome(List(1,2,2,1)))
  //println(isPalindrome(List(1,2,3,4)))
  //println(isPalindrome(List(1,2,3,2,1)))
  //println(isPalindrome(List(1,2,3,4,5)))

  /*
  Problem 7
  (**) Flatten a nested list structure.

  Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

  Example:

  * (my-flatten '(a (b (c d) e)))
  (A B C D E)
   */
  sealed trait NestedLista [+A]
  case class Elemento[A](a:A) extends NestedLista[A]
  case class Lista[A](lst:List[NestedLista[A]]) extends NestedLista[A]
/*
  val nl1 = Lista(List( Elemento(1), Lista(List(Elemento(2))), Elemento(3) ))
  println(nl1)

  def myFlatten[A](lst:Lista[NestedLista[A]]):List[A] = {
    def myFlattenR[A](lst:Lista[NestedLista[A]], acc: List[A]): List[A] = lst match {
      //case Nil => acc
      //case Elemento(n) :: xs => myFlattenR(xs , n :: acc)
      //case Lista(x) :: xs => myFlattenR(Lista(x),acc) :: myFlattenR(xs,acc)
    }
    myFlattenR(lst,Nil)
  }*/

  /*
  Problem 8
  (**) Eliminate consecutive duplicates of list elements.

  If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

  Example:

  * (compress '(a a a a b c c a a d e e e e))
  (A B C A D E)
  Example in Haskell:

  λ> compress "aaaabccaadeeee"
  "abcade"
   */

  def compress[A](lst:List[A]):List[A] = {
    @tailrec
    def compressR(lst:List[A],acc:List[A]):List[A] = lst match {
      case x :: y :: xs  => if(x!=y) compressR(y::xs,acc ::: List(x)) else compressR(y::xs, acc)
      case x :: Nil => acc ::: List(x)
      case Nil => acc
    }
    compressR(lst,Nil)
  }
  def compress2[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case head::Nil => List(head)
    case head::tail => if (head == tail.head) compress2(tail) else head::compress2(tail)
  }
//  println(compress(List(1,1,1,2)))
//  println(compress(List(1,1,1,2,2,2,3,3)))
  //println(compress(List(1,1,1,2,3,4,4,4,4,5,5)))

  /*
  Problem 9
  (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

  Example:

  * (pack '(a a a a b c c a a d e e e e))
  ((A A A A) (B) (C C) (A A) (D) (E E E E))
  Example in Haskell:

  λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
               'a', 'd', 'e', 'e', 'e', 'e']
  ["aaaa","b","cc","aa","d","eeee"]
   */
  def pack[A](lst:List[A]):List[List[A]] = {
    @tailrec
    def packR(lst: List[A],subAcc:List[A],acc:List[List[A]]):List[ List[A] ] = lst match {
      case Nil => acc
      case x :: Nil => packR(Nil,Nil,acc ::: List(x::subAcc))
      case x :: xs => if (x == xs.head) packR(xs,x::subAcc,acc) else packR(xs,Nil,acc ::: List(x::subAcc))
    }
    packR(lst,Nil,List())
  }
  //println(pack(List(1,1,1,1,2,3,3,4,4,4,5,5)))
  //println(pack(List(1,2)))



  /*
  Problem 10
  (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

  Example:

  * (encode '(a a a a b c c a a d e e e e))
  ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
  Example in Haskell:

  λ> encode "aaaabccaadeeee"
  [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
   */

  def encode[A](lst:List[A]):List[(Int,A) ] = {
    @tailrec
    def encodeR(lst: List[A],cont:Int,acc:List[(Int,A)]):List[ (Int,A) ] = lst match {
      case Nil => acc
      case x :: Nil => encodeR(Nil,0,acc ::: List((cont+1,x)))
      case x :: xs => if (x == xs.head) encodeR(xs,cont+1,acc) else encodeR(xs,0,acc ::: List((cont+1,x)))
    }
    encodeR(lst,0,List())
  }

//  println(encode(List(1,1,1,1,2,3,3,4,4,4,5,5)))
//  println(encode(List("a","b")))

  /*
  Problem 11
  (*) Modified run-length encoding.

  Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

  Example:

  * (encode-modified '(a a a a b c c a a d e e e e))
  ((4 A) B (2 C) (2 A) D (4 E))
  Example in Haskell:

  λ> encodeModified "aaaabccaadeeee"
  [Multiple 4 'a',Single 'b',Multiple 2 'c',
   Multiple 2 'a',Single 'd',Multiple 4 'e']
   */
  sealed trait Valores[+A]
  case class Multiple[A](veces:Int, element: A) extends Valores[A]
  case class Unico[A](element: A) extends Valores[A]

  def encodeModified[A](lst:List[A]):List[Valores[A]] = {
    @tailrec
    def encodeModifiedR(lst: List[A],cont:Int,acc:List[Valores[A]]):List[Valores[A]] = lst match {
      case Nil => acc
      case x :: Nil => if (cont > 0 ) encodeModifiedR(Nil,0,acc ::: List(Multiple(cont+1,x)))
                          else encodeModifiedR(Nil,0,acc ::: List(Unico(x)))
      case x :: xs => if (x == xs.head) encodeModifiedR(xs,cont+1,acc) else
        if (cont > 0 ) encodeModifiedR(xs,0,acc ::: List(Multiple(cont+1,x)))
          else encodeModifiedR(xs,0,acc ::: List(Unico(x)))
    }
    encodeModifiedR(lst,0,List())
  }

  //println(encodeModified(List(1,1,1,2,3,3,4)))
  //println(encodeModified(List(1,1,1,2,3,3,4,5,5)))

  /*
  Problem 12
  (**) Decode a run-length encoded list.

  Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

  Example in Haskell:

  λ> decodeModified
         [Multiple 4 'a',Single 'b',Multiple 2 'c',
          Multiple 2 'a',Single 'd',Multiple 4 'e']
  "aaaabccaadeeee"
   */
  def decodeModified[A](lst:List[Valores[A]]):List[A] = {
    def valToList(lst:Valores[A]):List[A] = lst match {
      case Unico(x) => List(x)
      case Multiple(n,ele) => List.fill(n)(ele)
    }
    @tailrec
    def decodeModifiedR(lst: List[Valores[A]],acc:List[A]):List[A] = lst match {
      case Nil => acc
      case x :: xs => decodeModifiedR(xs, acc ::: valToList(x) )
    }
    decodeModifiedR(lst,List())
  }

  //println(decodeModified(List(Multiple(3,1), Unico(2), Multiple(2,3), Unico(4) )))
  //println(decodeModified(List(Multiple(3,1), Unico(2), Multiple(2,3), Unico(4), Multiple(2,5))))

  /*
  Problem 13
  (**) Run-length encoding of a list (direct solution).

  Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

  Example:

  * (encode-direct '(a a a a b c c a a d e e e e))
  ((4 A) B (2 C) (2 A) D (4 E))
  Example in Haskell:

  λ> encodeDirect "aaaabccaadeeee"
  [Multiple 4 'a',Single 'b',Multiple 2 'c',
   Multiple 2 'a',Single 'd',Multiple 4 'e']
   */
/*
  def encodeDirect[A](lst:List[A]):List[Valores[A]] = {
    @tailrec
    def encodeDirectR(lst: List[A],cont:Int,acc:List[Valores[A]]):List[Valores[A]] = lst match {
      case Nil => acc
      case x :: Nil => if (cont > 0 ) encodeDirectR(Nil,0,acc ::: List(Multiple(cont+1,x)))
      else encodeDirectR(Nil,0,acc ::: List(Unico(x)))
      case x :: xs => if (x == xs.head) encodeDirectR(xs,cont+1,acc) else
        if (cont > 0 ) encodeDirectR(xs,0,acc ::: List(Multiple(cont+1,x)))
        else encodeDirectR(xs,0,acc ::: List(Unico(x)))
    }
    encodeDirectR(lst,0,List())
  }*/

  /*
  Problem 14
  (*) Duplicate the elements of a list.

  Example:

  * (dupli '(a b c c d))
  (A A B B C C C C D D)
  Example in Haskell:

  λ> dupli [1, 2, 3]
  [1,1,2,2,3,3]
   */
  def dupli[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case x :: xs => x :: x :: dupli(xs)
  }
  //println(dupli(List(1,2,3)))
  //println(dupli(List("a","b","c","c","d")))

  /*
  Problem 15
  (**) Replicate the elements of a list a given number of times.

  Example:

  * (repli '(a b c) 3)
  (A A A B B B C C C)
  Example in Haskell:

  λ> repli "abc" 3
  "aaabbbccc"
   */
  def repli[A](lst:List[A],cant:Int):List[A] = {
    def repliAux(ele:A, cant:Int):List[A] = cant match {
      case 0 => Nil
      case n => ele :: repliAux(ele, n-1)
    }
    def repliR(lst:List[A],cant:Int):List[A] = lst match {
      case Nil => Nil
      case x :: xs => repliAux(x,cant) ::: repliR(xs,cant)
    }
    repliR(lst,cant)
  }

  //println(repli(List("a","b","c"),3))
  //println(repli(List("a","b","c"),5))
  //println(repli(List("a","b","c"),0))

  /*
  Problem 16
  (**) Drop every N'th element from a list.

  Example:

  * (drop '(a b c d e f g h i k) 3)
  (A B D E G H K)
  Example in Haskell:

  λ> dropEvery "abcdefghik" 3
  "abdeghk"
   */
  def drop[A](lst:List[A],pos:Int):List[A] = {
    def dropR(lst:List[A],act:Int):List[A] = (lst,act) match {
      case (Nil,_) => Nil
      case (_::xs,1) => dropR(xs,pos)
      case (x::xs,n) => x :: dropR(xs,n-1)
    }
    dropR(lst,pos)
  }
  //println(drop(List(1,2,3,4,5,6,7,8,9),3))
  //println(drop(List("a","b","c","d","e","f","g","h","i","k"),3))

  /*
  Problem 17
  (*) Split a list into two parts; the length of the first part is given.

  Do not use any predefined predicates.

  Example:

  * (split '(a b c d e f g h i k) 3)
  ( (A B C) (D E F G H I K))
  Example in Haskell:

  λ> split "abcdefghik" 3
  ("abc", "defghik")
   */

  def split[A](lst:List[A],pos:Int):List[List[A]] = {
    @tailrec
    def splitAux(pos:Int,lst:List[A],lst2:List[A]):List[List[A]] = (pos,lst) match {
      case (_,Nil) => List(lst2,Nil)
      case (0,lst) => List(lst2,lst)
      case (a,x :: xs) => splitAux(a-1,xs, lst2 ::: List(x))
    }
    splitAux(pos,lst,Nil)
  }

  //println(split(List("a","b","c","d","e","f","g","h","i","k"),3))
  //println(split(List("a","b","c","d","e","f","g","h","i","k"),23))
  //println(split(List("a","b","c","d","e","f","g","h","i","k"),0))
  //println(split(List("a","b","c","d","e","f","g","h","i","k"),1))

  /*
  Problem 18
  (**) Extract a slice from a list.

  Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

  Example:

  * (slice '(a b c d e f g h i k) 3 7)
  (C D E F G)
  Example in Haskell:

  λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
  "cdefg"
   */
  def slice[A](lst:List[A],i:Int,k:Int):List[A] = {
    def sliceR(lst:List[A],pos:Int):List[A] = lst match {
      case Nil => Nil
      case _::_ if pos > k => Nil
      case x::xs => if(pos < i || pos > k) sliceR(xs,pos+1) else x :: sliceR(xs,pos+1)
    }
    sliceR(lst,1)
  }
  //println(slice(List("a","b","c","d","e","f","g","h","i","k"),3,7))
  //println(slice(List("a","b","c","d","e","f","g","h","i","k"),0,18))
  //println(slice(List("a","b","c","d","e","f","g","h","i","k"),10,11))

  /*
  Problem 19
  (**) Rotate a list N places to the left.

  Hint: Use the predefined functions length and (++).

  Examples:

  * (rotate '(a b c d e f g h) 3)
  (D E F G H A B C)

  * (rotate '(a b c d e f g h) -2)
  (G H A B C D E F)
  Examples in Haskell:

  λ> rotate ['a','b','c','d','e','f','g','h'] 3
  "defghabc"

  λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
  "ghabcdef"
   */
  def rotate[A](lst:List[A],pos:Int):List[A] = {
    if (pos >= 0) {
      val tupleL = split(lst,pos)
      tupleL(1) ::: tupleL.head
    }
    else {
      val tupleL = split(lst,lst.length + pos)
      tupleL(1) ::: tupleL.head
    }
  }
  def rotate2[A](lst:List[A],pos:Int):List[A] = {
    @tailrec
    def rotateR(lst:List[A],lst2:List[A],pos:Int):List[A] = pos match {
      case 0 => lst ::: lst2
      case n => rotateR(lst.tail,lst2 ::: List(lst.head) ,n-1)
    }
    if (pos<0) rotateR(lst,Nil,lst.length + pos) else  rotateR(lst,Nil,pos)
  }

  //println(rotate(List("a","b","c","d","e","f","g","h"),3))
  //println(rotate(List("a","b","c","d","e","f","g","h"),-2))
  //println(rotate2(List("a","b","c","d","e","f","g","h"),3))
  //println(rotate2(List("a","b","c","d","e","f","g","h"),-2))

  /*
  Problem 20
  (*) Remove the K'th element from a list.

  Example in Lisp:

  * (remove-at '(a b c d) 2)
  (A C D)

   */
  def removeAt[A](lst:List[A],pos:Int):List[A] = lst match {
    case Nil => Nil
    case x :: xs => if(pos==1) xs else x::removeAt(xs,pos-1)
  }
  //println(removeAt(List("a","b","c","d"),2))
  //println(removeAt(List("a","b","c","d"),0))
  //println(removeAt(List("a","b","c","d"),10))

  /*
  Problem 21
  Insert an element at a given position into a list.

  Example:

  * (insert-at 'alfa '(a b c d) 2)
  (A ALFA B C D)
  Example in Haskell:

  λ> insertAt 'X' "abcd" 2
  "aXbcd"
   */
  def insertAt[A](ele:A,lst:List[A],pos:Int):List[A] = lst match {
    case Nil => if(pos>=1) ele :: Nil else Nil
    case x :: xs => if(pos==1) ele::x::xs else x::insertAt(ele,xs,pos-1)
  }

  //println(insertAt("alfa",List("a","b","c","d"),2))
  //println(insertAt("alfa",List("a","b","c","d"),6))
  //println(insertAt("alfa",List("a","b","c","d"),1))
  //println(insertAt("beta",List("a","b","c","d"),1))
  //println(insertAt("delta",List("a","b","c","d"),10))

  /*
  Problem 22
  Create a list containing all integers within a given range.

  Example:

  * (range 4 9)
  (4 5 6 7 8 9)
  Example in Haskell:

  λ> range 4 9
  [4,5,6,7,8,9]
   */
  def range(i:Int,k:Int):List[Int] = i match {
    case n => if(n == k) List(k) else n::range(i+1,k)
  }
  //println(range(4,9))

  /*
  Problem 23
  Extract a given number of randomly selected elements from a list.

  Example:

  * (rnd-select '(a b c d e f g h) 3)
  (E D A)
  Example in Haskell:

  λ> rnd_select "abcdefgh" 3 >>= putStrLn
  eda
   */
/*  def rndSelect[A](lst:List[A],cant:Int):List[A] = {
    val rnd = new scala.util.Random
    def rndSelectR(cant:Int):List[A] = cant match {
      case 0 => Nil
      case n => elementAt(lst,rnd.between(1, lst.length-1)) :: rndSelectR(n-1)
    }
    rndSelectR(cant)
  }*/
  //println(rndSelect(List(1,2,3,4,5,6,7,8,9,10),2))
  //println(rndSelect(List(1,2,3,4,5,6,7,8,9,10),10))

  /*
  Problem 24
  Lotto: Draw N different random numbers from the set 1..M.

  Example:

  * (rnd-select 6 49)
  (23 1 17 33 21 37)
  Example in Haskell:

  λ> diff_select 6 49
  [23,1,17,33,21,37]
   */
  def rndSelect(i:Int,k:Int):List[Int] = {
    val rnd = new scala.util.Random
    def rndSelectR(i:Int,k:Int):List[Int] = i match {
      case 0 => Nil
      case n =>rnd.between(0,k) :: rndSelectR(n-1,k)
    }
    rndSelectR(i,k)
  }
  //println(rndSelect(6,49))

  /*
  Problem 25
  Generate a random permutation of the elements of a list.

  Example:

  * (rnd-permu '(a b c d e f))
  (B A D C E F)
  Example in Haskell:

  λ> rnd_permu "abcdef"
  "badcef"
   */


}

