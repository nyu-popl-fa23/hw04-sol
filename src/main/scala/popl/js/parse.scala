package popl.js

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
import ast._
import Bop._, Uop._

import scala.util.matching.Regex

object parse extends JavaTokenParsers:
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  val reserved: Set[String] =
    Set("undefined", "true", "false", "null",
        "const", "var", "name", "ref", 
        "function", "return", "interface",
        "bool", "string", "number", "Undefined", "Null")
  
  def stmt: Parser[Expr] =
    rep(basicStmt) ~ opt(lastBasicStmt) ^^
    { case (sts: List[Expr])~lst => 
        val stmts = sts ++ lst
        if (stmts == Nil) Undefined 
        else stmts reduceRight {
          case (ConstDecl(v, e1, _), st2) => ConstDecl(v, e1, st2)
          case (st1, st2) => BinOp(Seq, st1, st2)
        }
    }
   
  def stmtSep: Parser[String] = ";"
 
  def basicStmt: Parser[Expr] =
    stmtSep ^^^ Undefined | 
    decl <~ stmtSep |
    expr <~ stmtSep
    
  def lastBasicStmt: Parser[Expr] =
    stmtSep ^^^ Undefined |
    "{" ~> stmt <~ "}" |
    decl |
    expr
  
  def decl: Parser[ConstDecl] =
    positioned(
        ("const" ~> ident <~ "=") ~ expr ^^ 
        { case s~e => ConstDecl(s, e, Num(0))})
  
  def expr: Parser[Expr] = commaExpr
  
  def commaExpr: Parser[Expr] =
    condExpr ~ rep("," ~> condExpr) ^^
      { case e~es => 
          (e :: es) reduceRight {
            case (e1, e2) => BinOp(Seq, e1, e2).setPos(e1.pos) 
          }
      }
  
  def condExpr: Parser[Expr] =
    (orExpr <~ "?") ~ (orExpr <~ ":") ~ orExpr ^^
    { case e1~e2~e3 => If(e1, e2, e3).setPos(e1.pos) } |
    orExpr
  
  def orExpr: Parser[Expr] =
    andExpr ~ rep("||" ~> andExpr) ^^ 
      { case e1~es => 
        es.foldLeft(e1){ case (e1, e2) => BinOp(Or, e1, e2).setPos(e1.pos) }
      }

  def andExpr: Parser[Expr] =
    eqExpr ~ rep("&&" ~> eqExpr) ^^ 
      { case e1~es => 
        es.foldLeft(e1){ case (e1, e2) => BinOp(And, e1, e2).setPos(e1.pos) }
      }

  def binExpr(e: Parser[Expr], bop: Parser[Bop]): Parser[Expr] =
    e ~ rep(bop ~ e) ^^
      { case e1~opes =>
        opes.foldLeft(e1) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) }
      }

  def eqOp: Parser[Bop] =
    "===" ^^^ Eq |
    "!==" ^^^ Ne
    
  def eqExpr: Parser[Expr] = binExpr(relExpr, eqOp)

  def relOp: Parser[Bop] =
    "<="  ^^^ Le |
    "<"   ^^^ Lt |
    ">="  ^^^ Ge |
    ">"   ^^^ Gt

  def relExpr: Parser[Expr] = binExpr(additiveExpr, relOp)

  def additiveOp: Parser[Bop] = 
    "+" ^^^ Plus | 
    "-" ^^^ Minus
      
  def additiveExpr: Parser[Expr] = binExpr(multitiveExpr, additiveOp)

  def multitiveOp: Parser[Bop] = 
    "*" ^^^ Times |
    "/" ^^^ Div
  
  def multitiveExpr: Parser[Expr] = binExpr(unaryExpr, multitiveOp)

  def unaryOp: Parser[Uop] =
    "-" ^^^ UMinus
     
  def unaryExpr: Parser[Expr] =
    positioned(unaryOp ~ primaryExpr ^^ { case uop~e => UnOp(uop, e) }) |
    printExpr
  
  def printExpr: Parser[Expr] =
    positioned("console.log(" ~> expr <~ ")" ^^ (e => Print(e))) |
    primaryExpr
    
  def primaryExpr: Parser[Expr] = 
    literalExpr |
    positioned(ident ^^ { Var.apply }) |
    "(" ~> expr <~ ")" |
    "{" ~> stmt <~ "}"
    
  override def ident: Parser[String] =
    super.ident ^? ({
      case id if !reserved(id) => id
    }, { id => s"$id is reserved." }) | 
    "$" ~> super.ident ^^ (s => "$" + s)
    
  def literalExpr: Parser[Expr] =
    positioned("true" ^^^ Bool(true)) |
    positioned("false" ^^^ Bool(false)) |
    positioned("undefined" ^^^ Undefined) |
    positioned(floatingPointNumber ^^ { d => Num(d.toDouble) }) |
    positioned(stringLiteral ^^ (s => Str(s.substring(1, s.length() - 1))))
  
  override def stringLiteral: Parser[String] =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\"").r |
    ("\'"+"""([^'\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\'").r
  
  /** utility functions */
  private def getExpr(p: ParseResult[Expr]): Expr = 
    p match
      case Success(e, _) => 
        fv(e).headOption match
          case Some(x) => 
            throw new util.JsException(s"Unknown identifier $x", e.pos)
          case None => e
      case p : NoSuccess =>
        throw new util.JsException(p.msg, p.next.pos)

  
  def fromString(s: String): Expr = getExpr(parseAll(stmt, s))
  
  def fromFile(file: java.io.File): Expr =
    val reader = new java.io.FileReader(file)
    val result = parseAll(stmt, StreamReader(reader))
    getExpr(result)

