/*import java.net.Authenticator.RequestorType
import java.lang.annotation.Native
import java.lang.annotation.ElementType


object Math {
    val pi: Double = 3.14
    def sqrt(x: Int) = x * x
}


object Main extends App {
    def fact(n: Int, res: Int): Int = {
        if (n == 1) res
        else fact(n - 1, res * n)
    }
    def reverse(n: Int, res: Int): Int = {
        if(n == 0) res
        else reverse(n/10, res*10+(n%10))
    }
    def isPrime(n: Int, k: Int): Boolean = {
        if(n % k == 0) false
        else if(k >= n/2) true
        else isPrime(n, k + 1)
    }
    def bin(l: Int, r: Int, x: Int): Int = {
        val mid = (r + l) / 2
        if(mid <= 1) l
        else if(x*x > mid) bin(mid, r, x)
        else bin(l, mid, x)
    }
    def gcd(n: Int, m:Int): Int = {
        if(n == 0) m
        else if (m == 0) n
        else if(n > m) gcd(n % m, m)
        else gcd(n, m % n)
    }
    def repit(n: Int, str: String): String = {
        def loop(n: Int, s: String): String = {
            if(n == 0) s
            else loop(n - 1, str+s)
        }
        loop(n-1, str)
    }
    def isPSP(str: String): Boolean = {
        val n = str.size
        def loop(s: String, l: Int, r: Int): Boolean = {
            if(s.head == '(') loop(s.tail, l + 1, r)
            else if(s.head == ')') loop(str.slice(0, n - 1), l, r + 1)
            else if(s.isEmpty() && l == r) true
            else if(s.isEmpty() && l != r) false
            else loop(s.tail, l, r)
        }
        loop(str, 0, 0)
    }
    def reverse_str(str: String): String = {
        def loop(s: String, res: String): String = {
            if(s.isEmpty()) res
            else loop(s.tail, s.head + res)
        }
        loop(str, "")
    }
    def reWords(str: String): String = {
        val list = str.split(" ").toList
        def loop(l: List[String], res: String): String = {
            if(l.isEmpty) res
            else loop(l.tail, res + reverse_str(l.head) + ' ')
        }
        loop(list, "")
    }
    def reverse_list(list: List[ElementType]): List[ElementType] = {
        def loop(l: List[ElementType], res: List[ElementType]): List[ElementType] = {
            if(l.isEmpty) res
            else loop(l.tail,  l.head +: res)
        }
        loop(list, List.empty)
    }
    def del(list: List[Int]): List[Int] = {
        def loop(l: List[Int], res: List[Int], x: Int): List[Int] = {
            if(l.isEmpty) res
            else if(x == l.head) loop(l.tail, res, l.head)
            else loop(l.tail, res :+ l.head, l.head)
        }
        if(list.nonEmpty)
            list.head +: loop(list.tail, List.empty, list.head)
        else
            list
    }
    def concat(l: List[Int], r: List[Int]): List[Int] = {
        reverse_list(l)
        reverse_list(r)
        def loop(list: List[Int], l: List[Int]): List[Int] = {
            if(l.isEmpty) list
            else loop(list :+ l.head, l.tail)
        }
        loop(l, r)
    }
    def taken(list: List[Int], num: Int): List[List[Int]] = {
        def add(l: List[Int], n: Int, res: List[Int]): List[Int] = {
            if(l.isEmpty) res
            else add(l.tail, n - 1, res :+ l.head)
        }
        List(add(list, num, List.empty), List(10, 1))
    }
    def groupt(list: List[Int], group_num: Int): List[List[Int]] = {
        def loop(l: List[Int], group: List[Int], res: List[List[Int]]): List[List[Int]] = {
            if(l.isEmpty) (if (group.nonEmpty) res :+ group else res)
            else if(group.length >= group_num) loop(l.tail, List(l.head), res :+ group)
            else loop(l.tail, group :+ l.head, res)
        }
        loop(list, List.empty, List.empty)
    }
    def flatten(list: List[List[Int]]): List[Int] = {
        def loop(l: List[List[Int]],group: List[Int], res: List[Int]): List[Int] = {
            if(l.isEmpty && group.isEmpty) res
            else if(group.isEmpty) loop(l.tail, l.head, res)
            else loop(l, group.tail, res :+ group.head)
        }
        loop(list, List.empty, List.empty)
    }
    def pair_count(list: List[Int]): List[List[Int]] = {
        def loop(l: List[Int],num: Int ,count: Int, res: List[List[Int]]): List[List[Int]] = {
            if(l.isEmpty) res :+ List(num, count)
            else if(l.head == num) loop(l.tail, num, count + 1, res)
            else loop(l, l.head, 0, res :+ List(num, count))
        }
        loop(list, list.head, 0, List.empty)
    }
    //[1,1,1,2,2,1] -> [(1, 3), (2, 2), (1, 1)]
    println(fact(5, 1))
    println(reverse(105,0))
    println(isPrime(17, 2))
    println(gcd(6, 9))
    println(repit(3, "ab"))
    println(reverse_str("abc"))
    println(reWords("adc dca cdp"))
    val list: List[Int] = List(1, 1, 3, 4, 5, 6, 6, 7, 7)
    println(reverse_list(list))
    println(del(List.empty))
    println(concat(list, list))
    println(groupt(list, 4))
    val l: List[List[Int]] = List(List(1, 1), List(7, 7), List(9, 9 , 0, 0, 1, 1))
    println(flatten(l))
    val array: List[Int] = List(1, 1, 1, 2, 2, 1, 1, 0, -1)
    println(pair_count(array))
    class MyClass {
        val a = "abc"
        def plus(x: Int): Int = x + 1
    }
    val c: MyClass = new MyClass()
    println(c.plus(10))
    class Point(x: Int, y: Int) {
        def dist():Int = x*x + y*y
    }
    val p: Point = new Point(5, 5)
    println(Math.pi)
    val i = 42
    val s = i match{
        case 43 => "Number"
        case _ => "nothing"
    }
    println(s)
    println(fromInt(5))

    println(toInt(fromInt(5)))
}

//User:
//or Anoymous = sessionid: String
//or Regular = email: String and level: Level
//or Admin = id: Int and permis: Permissions

//Level:
//or newoie
// or Experienced: Coolest: Int
//Permissions:
//or Register
//re Ban

trait User
case class Anonymous(sessionid: String) extends User
case class Regular(email: String, level: Level) extends User
case class Admin(id: Int, permis: Permission) extends User

trait Level
case object newoie extends Level
case class Experienced(Coolest: Int) extends Level

trait Permission
case object Register extends Permission
case object Ban extends Permission


// + или
// * - и


//Request
// Header and Body
//Header
// method and size
//method
// Get or Post
//Body
// stringBody: String or BinBody: Int
case class Request(header: Header, body: Body)
case class Header(method: Method, size: Size) 
case class Size(value: Int)
trait Body
case class StringBody(value: String) extends Body
case class BinBody(value: Int) extends Body
trait Method
case object Get extends Method
case object Post extends Method

class House(build: Build){
    
}
case class Build(t: Type, res: Res,var hp: Int)
trait Type
case object Wall extends Type
case object Roof extends Type
trait Res
case object wood extends Res
case object stone extends Res
case object metal extends Res
case object hqm extends Res


sealed trait NyNothing

case object MyUnit

trait MyBool
case object MyTrue extends MyBool
case object MyFalse extends MyBool

/*def myif(b: MyBool, t: Int, f: Int): Int = {
    t match {
        case MyTrue => t
        case MyFalse => f
    }
}*/
*/

