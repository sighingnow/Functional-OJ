import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

object Solution {

    abstract class Expr
    case class Lit(value: Int) extends Expr
    case class Atom(name: String) extends Expr
    case class Add(lhs: Expr, rhs: Expr) extends Expr
    case class Mult(lhs: Expr, rhs: Expr) extends Expr
    case class Let(assigns: List[(Atom, Expr)], body: Expr) extends Expr

    class ExprParser extends RegexParsers {
        def lit: Parser[Lit] = "(\\+|\\-)?[0-9]+".r ^^ (x => Lit(x.toInt))
        def atom: Parser[Atom] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ (x => Atom(x))
        def add: Parser[Add] = "(" ~ "add" ~ expr ~ expr ~ ")" ^^ {
            case _ ~ _ ~ lhs ~ rhs ~ _ => Add(lhs, rhs)
        }
        def mult: Parser[Mult] = "(" ~ "mult" ~ expr ~ expr ~ ")" ^^ {
            case _ ~ _ ~ lhs ~ rhs ~ _ => Mult(lhs, rhs)
        }
        def let: Parser[Let] = "(" ~ "let" ~ ((atom ~ expr) +) ~ expr ~ ")" ^^ {
            case _ ~ _ ~ kvs ~ body ~ _ => {
                val assigns = kvs.map(_ match {
                    case name ~ value => (name, value)
                })
                Let(assigns, body)
            }
        }
        def expr: Parser[Expr] = lit | atom | add | mult | let
    }

    type Env = HashMap[String, Int]

    def eval(env: Env, expr: Expr): Int = expr match {
        case Lit(value) => value
        case Atom(name) => env.get(name).get
        case Add(lhs, rhs) => eval(env, lhs) + eval(env, rhs)
        case Mult(lhs, rhs) => eval(env, lhs) * eval(env, rhs)
        case Let(assigns, body) => {
            val nenv = assigns.foldLeft(env)((envx, assign) => assign match {
                case (Atom(name), value) => envx + (name -> eval(envx, value))
            })
            eval(nenv, body)
        }
    }

    def evaluate(expression: String): Int = {
        val parser = new ExprParser
        val env = HashMap.empty[String, Int]
        eval(env, parser.parseAll(parser.expr, expression).get)
    }

    def test(): Unit = {
        println(evaluate("(add 1 2)")) // Output: 3

        println(evaluate("(mult 3 (add 2 3))")) // Output: 15

        println(evaluate("(let x 2 (mult x 5))")) // Output: 10

        println(evaluate("(let x 2 (mult x (let x 3 y 4 (add x y))))")) // Output: 14

        println(evaluate("(let x 3 x 2 x)")) // Output: 2

        println(evaluate("(let x 1 y 2 x (add x y) (add x y))")) // Output: 5

        println(evaluate("(let x 2 (add (let x 3 (let x 4 x)) x))")) // Output: 6

        println(evaluate("(let a1 3 b2 (add a1 1) b2)")) // Output 4

        println(evaluate("(let x 7 -12)")) // Output -12
    }
}
