import scala.util.CommandLineParser.FromString.given_FromString_Int
object Monoid extends App{
    trait Monoid[A] {
        def add(a: A, b: A): A
        def empty: A
    }
    val intMul: Monoid[Int] = new Monoid[Int]{
        def empty: Int = 1
        def add(a: Int, b: Int):Int = a * b
    }
    val intAdd: Monoid[Int] = new Monoid[Int] {
        def empty: Int = 0
        def add(a: Int, b: Int):Int = a + b
    }
    val strCon: Monoid[String] = new Monoid[String]{
        def empty: String = ""
        def add(a: String, b: String): String = a + b
    }
    def addAll[A](list: List[A])(proof: Monoid[A]): A = {
        def loop(l: List[A], res: A): A = 
            l match
                case Nil => res
                case head :: tail => loop(tail, proof.add(head, res))
        loop(list, proof.empty)
    }
    
    trait Show[A]{
        def show(v: A): String
    }
    given showInt: Show[Int] = new Show[Int]{
        def show(v: Int): String = 
            "Integer: " + v.toString()
    }
    given showStr: Show[String] = new Show[String]{
        def show(v: String): String = 
            "String: " + v
    }
    given showPair[A, B](using proofA: Show[A], proofB: Show[B]): Show[(A, B)] = new Show[(A, B)]{
        def show(v: (A, B)): String = "Pair: " +  proofA.show(v._1) + ", " + proofB.show(v._2) 
    }
    given showList[A](using proof: Show[A]): Show[List[A]] = new Show[List[A]] {
        def show(v: List[A]): String = 
            "List: " + v.map(a => proof.show(a)).mkString(", ")
    }
    def showall[A](list: List[A])(using proof: Show[A]): String = {
        def loop(l: List[A], res: String): String = 
            l match 
                case Nil => res
                case head :: tail => loop(tail, res + " " + proof.show(head) + ", ")
        loop(list, "")
    }
    val l = List(1, 0, 15)
    println(showall(l))

    given pairMonoid[A, B](using proofA: Monoid[A], proofB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        def add(a: (A, B), b: (A, B)): (A, B) =
            (proofA.add(a._1, b._1),
            proofB.add(a._2, b._2))
        def empty: (A, B) = (proofA.empty, proofB.empty)
    }
    val proof = summon[Show[(Int, String)]]
    println(proof.show(42, "abc"))
    val list = summon[Show[List[Int]]]
    println(list.show(List(1, 2, 3, 4)))
}
