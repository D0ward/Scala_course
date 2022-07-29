object Newmain extends App{
    /*def func(x: Int, y: Int): Int = x + y
    val f: (Int, Int) => Int = func
    val f2: (Int, Int, Int, Int) => Int = (x: Int, y: Int, z: Int, k: Int) => {
        x + y + z + k
    }
    val f3: (Int, Int, Int, Int) => Int = (x, y, z, k) => x + y + z + k
    println(f2(2, 3, 4, 5))
    println(f3)
    def example(f: (Int, Int) => Int, g: Int => String): (Int, Int) => String = 
        (x, y) => g(f(x, y))
    val k = example((x, y) => x + y,
    z => "Result: " + z.toString())
    println(k(10, 15))
    def repeat(func: Int => Int, num: Int): Int => Int ={
        def loop(n: Int, res: Int => Int): Int => Int ={
            if(n == 0) res
            else {
                val fminus1 = loop(n-1, func)
                val result: Int => Int = x =>
                    val fxminus1 = func(fminus1(x))
                    //val            
            }
        }
        loop(num, x => x)
    }
    val f4: Int => Int = x => x+1
    //val fun = repeat(f4(10), 5)
    //println(fun(10))
    def repeat_A[A](func: A => A, num: Int): A => A ={
        if(num == 1) func
        if(num == 0) identity(func)
        else{
            repeat_A(func => A, num - 1)
        }
    }
    def repeat[A](func: A => A, num: Int): A => A = {
        num match
            case 1 => func
            case _ => repeat(func, num - 1)
    }
    val fun: Int => Int = x => x + 1
    println(repeat(fun, 3))
    val fun1 = repeat(fun, 3)
    println(fun(3))
    def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))*/
    def map[A, B](list: List[A], func: A => B): List[B] ={
        def loop(l: List[A], res: List[B]): List[B] = {
            l match
                case Nil => res
                case head :: tail => loop(tail, res :+ func(head))
        }
        loop(list, List.empty)
    }
    val list: List[Int] = List(1, 2, 3, 4, 5)
    val fun_str: Int => String = x => "abc" + x.toString()
    println(map(list, fun_str))
    def filter[A](list: List[A], pred: A => Boolean): List[A] = {
        def loop(l: List[A], res: List[A]): List[A] = {
            l match 
                case Nil => res
                case head :: tail => {
                    if(pred(head)) loop(tail, res :+ head)
                    else loop(tail, res)
                }
        }
        loop(list, List.empty)
    }
    def foral[A](list: List[A], pred: A => Boolean): Boolean = {
        def loop(l: List[A], res: Boolean): Boolean = {
            l match 
                case Nil => res
                case head :: tail => {
                    if(pred(head)) loop(tail, res)
                    else false
                }
        }
        loop(list, true)
    }
    def fold[A, B](list: List[A], base: B, step: (A, B) => B): B = {
        list match 
            case Nil => base
            case head :: tail => fold(tail, step(head, base), step)
    }
    def flatMapp[A, B](list: List[A], func: A => List[B]): List[B] = {
        list.map(func).flatten
    }
    val fil: Int => Boolean = x => {
        if(x % 2 == 0) true
        else false
    }
    println(filter(list, fil))
    println(foral(list, fil))
    println(foral(filter(list, fil), fil))
    val l: List[Int] = List(2, 3, 4, 5, 6)
    val s: (Int, Int) => Int = (a, b) => b * a * (a - 1)
    val rev: (Int, List[Int]) => List[Int] = (x, y) => x +: y
    val l2 = fold(l, List.empty , rev)
    println(l2)
    val con: (Int, List[Int]) => List[Int] = (x, y) => y :+ x
    println(fold(l2, l, con)) 

    def fla[A](x: List[A], y: List[A]): List[A] =  y.concat(x)
    val flat = fla
    val lis: List[List[Int]] = List(List(1, 1), List(7, 7), List(9, 9 , 0, 0, 1, 1))
    println(fold(lis, Nil, flat))

    
    type withLogs[A] = (String, A)

    def minus1(num: Int): withLogs[Int] = 
        ("minus1 " + num.toString() , num - 1)
    def plus1(num: Int): withLogs[Int] =
        ("plus1 " + num.toString() + '\n', num+1)
    
    def first(a: Int): withLogs[String] ={
        ("fisrt num = " + a.toString() + '\n', a.toString())
    }
    def second(b: String): withLogs[String] = {
        ("second actions ", b + " - is b")
    }
    def compose_withlogs[A, B, C](f: A => withLogs[B], g: B => withLogs[C]): A => withLogs[C] = a =>{
        val (logF, valueF) = f(a)
        val (logG, valueG) = g(valueF)
        (logF+logG, valueG ) 
    }
    val f = compose_withlogs(first, second)
    val (log, res) = f(4)
    println(log)
    println(res)
}