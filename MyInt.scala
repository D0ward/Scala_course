import scala.concurrent.duration.fromNow

trait  Nat
case object Zero extends Nat
case class Succ(Prev: Nat) extends Nat

def fromInt(i: Int): Nat = {
    i match {
        case 0 => Zero
        case _ => Succ(fromInt(i-1))
    }
}

def plus(x: Nat, y: Nat): Nat = {
    def loop(n: Nat, m: Nat, res: Nat): Nat ={
        n match {
            case Zero => m match {
                case Zero => res
                case Succ(m) => loop(n, m, Succ(res))
            }
            case Succ(n) => loop(n, m, Succ(res))
        }
        
        
    }
    loop(x, y, Zero)
}

def umn(x: Nat, y: Nat): Nat = {
    def loop(n: Nat, m: Nat, res: Nat): Nat = {
        m match{
            case Zero => res
            case Succ(m) => {
                loop(n, m, plus(x, y))
            }
        }
    }
    loop(x, y, Zero)
}

def minus(x: Nat, y:Nat): Nat = {
    y match{
        case Zero => x
        case Succ(y) => x match {
            case Zero => Zero
            case Succ(x) => minus(x, y)
        }
    }
}

def geq(a: Nat, b: Nat): Boolean = {
    (a, b) match {
        case (Zero, Zero) => true
        case (Zero, b) => false
        case (a, Zero) => false
        case _ => geq(minus(a, b), minus(b, a))
    }
}

def del(a: Nat, b: Nat): Nat = {
    (a, b) match{
        case (Zero, b) => Zero
        case (a, Zero) => Zero
        case _ => {
            def loop(m: Nat, res: Nat): Nat = {
                m match{
                    case Zero => res
                    case Succ(m) => loop(minus(m,a), Succ(res))
                }
            }
            loop(b, Zero)
        }
    }
}

def toInt(i: Nat): Int = {
    def loop(num: Nat, res: Int): Int = {
        num match {
            case Zero => res
            case Succ(num) => loop(num ,res + 1)
        }
    }
    loop(i, 0)
}

object MyInt extends App {
    println(plus(fromInt(5), fromInt(5)))
    println(minus(fromInt(6), fromInt(4)))
    println(umn(fromInt(3), fromInt(2)))
    println(geq(fromInt(2), fromInt(2)))
    println(del(fromInt(10), fromInt(5)))
}
