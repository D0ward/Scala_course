object Normal_list extends App{

    trait MyList[+ElemType]
    case object Nil extends MyList[Nothing]
    case class Cons[ElemType](head: ElemType, tail: MyList[ElemType]) extends MyList[ElemType]
    def prepend[A](list: MyList[A], x: A): MyList[A] = {
            Cons(x, list)
        }
    def append[A](list: MyList[A], x: A): MyList[A] = {
        list match{
            case Nil => Cons(x, Nil)
            case Cons(h, t) => prepend(append(t, x), h) 
        }
    }
    def reverse_myList[A](list: MyList[A]): MyList[A] = {
        def loop(l: MyList[A], res: MyList[A]): MyList[A] = {
            l match{
                case Nil => res
                case Cons(head, tail) => loop(tail, prepend(res, head))
            }
        }
        loop(list, Nil)
    }
    def concat_mylist[A](l: MyList[A], r: MyList[A]): MyList[A] = {
        def loop(list: MyList[A], l: MyList[A]): MyList[A] = {
            l match{
                case Nil => list
                case Cons(h, t) => loop(append(list, h), t)
            }
        }
        loop(l, r)
    }
    def taken_mylist[A](list: MyList[A], num: Int): MyList[A] = {
        def add(l: MyList[A], n: Int, res: MyList[A]): MyList[A] = {
            l match{
                case Nil => res
                case Cons(head, tail) => add(tail, n - 1, append(res, head))
            }
        }
        add(list, num, Nil)
    }
    def map_mylist[A, B](list: MyList[A], func: A => B): MyList[B] ={
        def loop(l: MyList[A], res: MyList[B]): MyList[B] = {
            l match
                case Nil => res
                case Cons(head, tail) => loop(tail, append(res, func(head)))
        }
        loop(list, Nil)
    }
    def filter_mylist[A](list: MyList[A], pred: A => Boolean): MyList[A] = {
        def loop(l: MyList[A], res: MyList[A]): MyList[A] = {
            l match 
                case Nil => res
                case Cons(head, tail) => {
                    if(pred(head)) loop(tail, append(res, head))
                    else loop(tail, res)
                }
        }
        loop(list, Nil)
    }
    def foral_mylist[A](list: MyList[A], pred: A => Boolean): Boolean = {
        def loop(l: MyList[A], res: Boolean): Boolean = {
            l match 
                case Nil => res
                case Cons(head, tail) => {
                    if(pred(head)) loop(tail, res)
                    else false
                }
        }
        loop(list, true)
    }
    def fould_mylist[A, B](list: MyList[A], base: B, step: (A, B) => B): B = {
        list match 
            case Nil => base
            case Cons(head, tail) => fould_mylist(tail, step(head, base), step)
    }
    val fil: Int => Boolean = x => {
        if(x % 2 == 0) true
        else false
    }
    
    val list: MyList[Int] = Cons(5, Cons(6, Cons(10, Nil)))
    println(reverse_myList(list))
    println(concat_mylist(list, list))
    println(taken_mylist(list, 1))
    val fun_str: Int => String = x => "abc" + x.toString()
    println(map_mylist(list, fun_str))
    println(filter_mylist(list, fil))
    println(foral_mylist(list, fil))
    println(foral_mylist(filter_mylist(list, fil), fil))
}

//+A
//1. Derived extends Base
//2. MyList [Derived] extends MyList[Base]
