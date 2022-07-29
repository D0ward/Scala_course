import scala.io.StdIn.readLine
import javax.management.monitor.Monitor
import MyList.Cons

object file extends App{
    trait ToyIO[A]{
        def flatMap[B](f: A => ToyIO[B]): ToyIO[B] =
            FlatMap(this, f)
        def map[B](f: A => B): ToyIO[B] = {
            flatMap(a => ToyIO.pure(f(a)))
        }
    }

    case class Value[A](value: A) extends ToyIO[A]
    case class Effect[A](run: () => A) extends ToyIO[A]
    case class FlatMap[A, B](effect: ToyIO[A], f: A => ToyIO[B]) extends ToyIO[B]
    
    object ToyIO {
        def pure[A](value: A): ToyIO[A] = 
            Value(value)
        def delay[A](effect: => A): ToyIO[A] =
            Effect(() => effect)
        def unsafeRun[A](io: ToyIO[A]): A =
            io match{
                case Value(value) => value
                case Effect(run) => run()
                case FlatMap(eff, f) => {
                    val res = unsafeRun(eff)
                    val continuation = f(res)
                    unsafeRun(continuation)
                }
            }
        val getStrLn = delay{
            readLine()
        }
        def putInt(int: Int): ToyIO[Int] = 
            delay{int}
        def putStrln(str: String): ToyIO[Unit] =
            delay{println(str)}
    }
    
    trait Monad[F[_]]{
        def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
        def pure[A](value: A): F[A]
        def map[A, B](fa: F[A], f: A => B): F[B] =  flatMap(fa, x => pure(f(x)))
    }
  
    given Monad[ToyIO] with{
        def flatMap[A, B](fa: ToyIO[A], f: A => ToyIO[B]): ToyIO[B] = fa.flatMap(f)
        def pure[A](value: A): ToyIO[A] = ToyIO.pure(value)
    }
  
    trait Console[F[_]]{
        def getStrLn: F[String]
        def putStrln(str: String): F[Unit]
    }

    given Console[ToyIO] with{
        def getStrLn: ToyIO[String] = ToyIO.getStrLn
        def putStrln(str: String): ToyIO[Unit] = ToyIO.putStrln(str)
    }

    def putStrln[F[_]](str: String)(using console: Console[F]): F[Unit] = console.putStrln(str)
    def getStrLn[F[_]](using console: Console[F]): F[String] = console.getStrLn

    extension [F[_], A](fa: F[A])(using monad: Monad[F]){
        def flatMap[B](f: A => F[B]): F[B] = monad.flatMap(fa, f)
        def map[B](f: A => B): F[B] = monad.map(fa, f)
    }

    def funcf[F[_]: Monad: Console](a: Int): F[String] = for{
        _ <- putStrln(a.toString())
        enter <- getStrLn
    } yield enter
    val res = funcf[ToyIO](42)
    ToyIO.unsafeRun(res)    
}
