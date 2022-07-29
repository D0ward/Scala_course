import scala.io.StdIn.readChar
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.util.Random

object GAme extends App{
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
        private val r = new Random()
        def random(n: Int): ToyIO[Int] =
            delay{r.nextInt(n)}
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
        val getInt = delay{
            readInt()
        }
        def putInt(int: Int): ToyIO[Int] = 
            delay{int}
        def putStrln(str: String): ToyIO[Unit] =
            delay{println(str)}
    }
    
    trait GameState
    case object InProgress extends GameState
    case object Won extends GameState
    case object Lose extends GameState

    case class Game(word: String, guesses: String, max: Int){
        def renderWord: String = {
            word.map(c => 
                if(guesses.contains(c)) c
                else '_'
            )
        }
        def addGuess(c: Char): Game = 
            if(guesses.contains(c))  this 
            else copy(guesses = guesses+c)
        def state: GameState = 
            if(guesses.filter(c => !word.contains(c)).length > max) Lose
            else if (word.forall(guesses.contains)) Won
            else InProgress
        def isnewguess(c: Char): (Game, String) = {
            if(guesses.contains(c))
                (this, "This char you write before")
            else if(word.contains(c))
                (addGuess(c), "Yeah")
            else{
                val fail = guesses.filter(c => !word.contains(c)).length
                val pic = fail match{
                    case 0 => "____________"
                    case 1 => {
                        "\t /|\n" +
                        "\t//|__________\n"
                    }
                    case 2 =>{
                        "\t  |\n"+
                        "\t  |\n"+
                        "\t /|\n" +
                        "\t//|__________\n"
                    }
                    case 3 =>{
                        "\t  |\n"+
                        "\t  |\n"+
                        "\t  |\n"+
                        "\t /|\n" +
                        "\t//|__________\n"
                    }
                    case 4 =>{
                        "\t   _______\n"+
                        "\t  |      \\\\\n"+
                        "\t  |\n"+
                        "\t  |\n"+
                        "\t /|\n" +
                        "\t//|__________\n"
                    }
                    case _ => {
                        "\t   _______\n"+
                        "\t  |      \\\\\n"+
                        "\t  |        ()\n"+
                        "\t  |       /||\\\n"+
                        "\t /|        ||\n" +
                        "\t//|________TT\n"
                    }


                }
                (addGuess(c), pic + '\n' +"Nope")
            } 
        }
    }
    val dict = List("about", "act", "actually", "add", "after", "again", "against", "age", "ago", "air", "all", "also",
        "always", "among", "and", "animal", "another", "answer", "appear", "are", "area", "ask", "back", "ball", "base",
        "beauty", "because", "become", "bed", "been", "before", "began", "begin", "behind", "best", "better", "better",
        "between", "big", "bird", "black", "blue", "boat", "body", "book", "both", "bottom", "box", "boy", "bring",
        "brought", "build", "built", "busy", "but", "call", "came", "can", "car", "care", "carefully", "carry", "centre",
        "certain", "change", "check", "child", "children", "city", "class", "clear", "close", "cold", "colour", "come",
        "common", "community", "complete", "contain", "could", "country", "course", "create", "cried", "cross", "cry",
        "cut", "dark", "day", "decide", "decided", "deep", "develop", "did", "different", "does", "dog", "door", "down",
        "draw", "dream", "drive", "dry", "during", "each", "early", "earth", "east", "easy", "eat", "effort", "enough",
        "every", "example", "experience", "explain", "eye", "face", "fact", "false", "family", "far", "farm", "fast",
        "father", "feel", "feet", "few", "field", "find", "fire", "first", "fish", "five", "fly", "follow", "food",
        "form", "found", "four", "friend", "from", "front", "full", "game", "gave", "get", "girl", "give", "gold", "good",
        "got", "government", "great", "green", "ground", "group", "grow", "guy", "had", "half", "hand", "happen",
        "happened", "hard", "has", "have", "hear", "heat", "heavy", "help", "her", "here", "high", "his", "hold", "home",
        "horse", "hot", "hour", "house", "hundred", "idea", "important", "inch", "include", "into", "island", "just",
        "keep", "kind", "king", "knew", "know", "known", "land", "language", "large", "last", "late", "later", "laugh",
        "lead", "learn", "leave", "left", "less", "less", "let", "letter", "life", "light", "like", "line", "list",
        "listen", "little", "live", "long", "look", "love", "low", "machine", "made", "make", "man", "many", "map",
        "mark", "may", "mean", "measure", "men", "might", "mile", "million", "mind", "minute", "miss", "money", "month",
        "moon", "more", "more", "morning", "most", "mother", "mountain", "move", "much", "music", "must", "name",
        "nation", "near", "need", "never", "new", "next", "night", "north", "note", "notice", "noun", "now", "number",
        "object", "off", "office", "often", "oil", "old", "once", "one", "only", "open", "order", "other", "our", "out",
        "over", "page", "pair", "part", "pass", "passed", "people", "perhaps", "person", "picture", "place", "plan",
        "plane", "plant", "play", "point", "power", "probably", "problem", "product", "provide", "pull", "put",
        "question", "quick", "rain", "ran", "reach", "read", "ready", "real", "receive", "record", "red", "relationship",
        "remember", "right", "river", "road", "rock", "room", "round", "rule", "run", "said", "same", "saw", "say",
        "school", "science", "sea", "season", "second", "see", "seem", "self", "sentence", "serve", "set", "several",
        "shape", "she", "ship", "short", "should", "show", "shown", "side", "simple", "since", "sing", "sit", "six",
        "size", "sleep", "slow", "small", "snow", "some", "something", "song", "soon", "sound", "south", "space",
        "special", "spell", "spring", "stand", "star", "start", "stay", "step", "stood", "stop", "story", "street",
        "strong", "study", "such", "summer", "sun", "system", "table", "take", "talk", "teach", "tell", "ten", "test",
        "than", "that", "the", "their", "them", "then", "there", "these", "they", "thing", "think", "this", "those",
        "though", "thought", "thousand", "three", "through", "time", "together", "told", "too", "took", "top", "toward",
        "town", "travel", "tree", "try", "true", "turn", "two", "under", "understand", "until", "upon", "use", "usual",
        "very", "voice", "vowel", "wait", "walk", "want", "war", "warm", "was", "watch", "water", "wave", "way", "week",
        "weight", "were", "west", "what", "wheel", "where", "which", "white", "who", "why", "will", "wind", "winter",
        "with", "without", "woman", "wonder", "wood", "word", "words", "work", "world", "would", "write", "wrong", "year",
        "yes", "you")

    def chooseword: ToyIO[(String, String)] =
        ToyIO.random(dict.length).map(index => (dict.apply(index), "start"))
    def inputChar: ToyIO[Char] = {
        for{
            _ <- ToyIO.putStrln("Enter guess: ")
            input <- ToyIO.getStrLn
            result <- if(input.length == 1)
                        ToyIO.pure(input.head)
                    else for{
                        _ <- ToyIO.putStrln("Invalie input")
                        result <- inputChar
                    } yield result
        } yield result
    }
    val draw = "\n\t  _________\n"+
               "\t  |       \\\\\n"+
               "\t  |        ()\n"+
               "\t  |       /||\\\n"+
               "\t /|        ||\n" +
               "\t//|__________\n"
    def playgame(game: Game): ToyIO[Unit] = for {
        _ <- ToyIO.putStrln("Word - " + game.renderWord)
        guess <- inputChar
        _ <- ToyIO.putStrln("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" + "Your guess: " + guess)
        (newGame, res) = game.isnewguess(guess)
        _ <- ToyIO.putStrln(res)
        _ <- newGame.state match {
            case InProgress => playgame(newGame)
            case Lose => ToyIO.putStrln("You die\n" + "This word - " + game.word + draw) 
            case Won => ToyIO.putStrln("You won\n" + "This word - " + game.word)
        }
    } yield ()

    val program = for {
        _ <- ToyIO.putStrln("Welcome to Hangman game!")
        chosen <- chooseword
        (word, info) = chosen
        _ <- ToyIO.putStrln(info)
        game = Game(word, "", word.length())
        _ <- playgame(game)
    } yield ()
    ToyIO.unsafeRun(program)
}  