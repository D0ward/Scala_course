object MyList extends App{

    trait IntList
    case object Empty extends IntList
    case class Cons(head: Int, tail: IntList) extends IntList

    def prepend(list: IntList, x: Int): IntList = {
            Cons(x, list)
        }
    def append(list: IntList, x: Int): IntList = {
        list match{
            case Empty => Cons(x, Empty)
            case Cons(h, t) => prepend(append(t, x), h) 
        }
    }
    def reverse_mylist(list: IntList): IntList = {
        def loop(l: IntList, res: IntList): IntList = {
            l match{
                case Empty => res
                case Cons(h, t) => loop(t, append(res, h))
            }
        }
        loop(list, Empty)
    }
    def del_mylist (list: IntList): IntList = {
        def loop(l: IntList, res: IntList, x: Int): IntList = {
            l match {
                case Empty => res
                case Cons(h, t) => if(x == h) loop(t, res, h)
                                    else loop(t, append(res, h), h)
                }
            }
        list match {
            case Empty => list
            case Cons(h, t) => append(loop(t, Empty, h), h)
        }
    }
    def concat_list(l: IntList, r: IntList): IntList = {
        def loop(list: IntList, l: IntList): IntList = {
            l match{
                case Empty => list
                case Cons(h, t) => loop(append(list, h), t)
            }
        }
        loop(l, r)
    }
    def taken_mylist(list: IntList, num: Int): IntList = {
        def add(l: IntList, n: Int, res: IntList): IntList = {
            l match{
                case Empty => res
                case Cons(head, tail) => add(tail, n - 1, append(res, head))
            }
        }
        add(list, num, Empty)
    }
    
    val list: IntList = Empty
    
    println(prepend(list, 4))
    println(prepend(list, 5))
    println(prepend(list, 6))
    println(list)
    println(append(list, 5))
    
    println(reverse_mylist(list))
    println(list)
}
