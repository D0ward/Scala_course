import scala.io.StdIn.readInt

object Newnewmain extends App{
    case class WithLogs[A] (logs: String, value: A){
        def flatMap[B](f: A => WithLogs[B]): WithLogs[B] = {
            val resf = f(value)
            WithLogs(resf.logs, resf.value)
        }
        def map[B](f: A => B): WithLogs[B] = {
            val resf = f(value)
            WithLogs(logs, resf)
        }
    }
    def log(str: String): WithLogs[Unit]=
        WithLogs(str, ())
    def compose_withlogs[A, B, C](f: A => WithLogs[B], g: B => WithLogs[C]): A => WithLogs[C] = a =>{
        val valueF = f(a)
        val valueG = g(valueF.value)
        WithLogs(valueF.logs + valueG.logs, valueG.value) 
    }
    def minus1(num: Int): WithLogs[Int] = 
        for{
            _ <- log("minus -1 + " + num.toString() + '\n')
        } yield num - 1
    def plus1(num: Int): WithLogs[Int] =
       for{
            _ <- log("Plus 1 + " + num.toString() + '\n')
       } yield num + 1
    val fun = compose_withlogs(minus1, plus1)
    val res = fun(4)
    println(res.logs)
    println(res.value)

    case class State[S, A](run: S => (S, A)){
        def flatMap[B](f: A => State[S, B]): State[S, B] = {
            val result: S => (S, B) =
                initState => {
                    val (s, a) = run(initState)
                    val fab = f(a)
                    fab.run(s)
                }
            State(result)
        }
        def map[B](f: A => B): State [S, B] = flatMap(x => pure(f(x)))
        
    } 
    def pure[S, A](value: A): State[S, A] = {
            val run: S => (S, A) = 
                state => (state, value)
            State(run)
        }
    def get[S](): State[S, S] = State(st => (st, st))
    //def set[S](s: S): State
    def func(a: Int): State[Int, String] ={
        State(i => 
            (i + 1, a.toString())
        )
    }
    def modify[S](f: S => S): State[S, Unit] = {
        State(st => (f(st), ()))
    }
    val program = func(10)
    val (state, re1s) = program.run(0)
    println(state)
    println(re1s)
    def whiles[S, A](cond: S => Boolean)(body: State[S, A]): State[S, Unit] = {
        get[S]().flatMap(state => 
            if(!cond(state)) pure(())
            else for{
                _ <- body
                _ <- whiles(cond)(body)
            } yield ()
        )
    }
    

}