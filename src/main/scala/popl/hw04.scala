package popl

object hw04 extends js.util.JsApp:
  import js.ast._
  import js._
  import Bop._, Uop._

  /*
   * CSCI-UA.0480-055: Homework 4
   */

  /*
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your solution will _not_ be graded if it does not compile!!
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   *
   */

  /* JakartaScript */
  
  // Value environments
  type Env = Map[String, Val]
  // Empty environment
  def emp: Env = Map()
  // env[x |-> v]
  def extend(env: Env, x: String, v: Val): Env = env + (x -> v)
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNum(v: Val): Double =
    v match
      case Num(n) => n
      case Bool(false) => 0
      case Bool(true) => 1
      case Undefined => Double.NaN
      case Str(s) => try s.toDouble catch { case _: Throwable => Double.NaN }

  
  def toBool(v: Val): Boolean =
    v match
      case Num(n) if (n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN => false
      case Num(_) => true
      case Bool(b) => b
      case Undefined => false
      case Str("") => false
      case Str(_) => true

  
  def toStr(v: Val): String =
    v match
      case Num(n) => if n.isWhole then "%.0f" format n else n.toString
      case Bool(b) => b.toString
      case Undefined => "undefined"
      case Str(s) => s

  
  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   */
  def inequalityVal(bop: Bop, v1: Val, v2: Val): Boolean =
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match
      case (Str(s1), Str(s2)) =>
        (bop: @unchecked) match
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
      case _ =>
        val (n1, n2) = (toNum(v1), toNum(v2))
        (bop: @unchecked) match
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2

  
  def eval(env: Env, e: Expr): Val =
    /* Some helper functions for convenience. */
    def eToNum(e: Expr): Double = toNum(eval(env, e))
    def eToBool(e: Expr): Boolean = toBool(eval(env, e))
    def eToVal(e: Expr): Val = eval(env, e)
    
    e match
      /* Base Cases */
      case e: Val => e
      
      case Var(x) => env(x)
      
      /* Inductive Cases */
      case Print(e) => println(eToVal(e).prettyVal); Undefined

      case UnOp(UMinus, e1) => Num(- eToNum(e1))
      
      case UnOp(Not, e1) => Bool(! eToBool(e1))
      
      case BinOp(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match
        case (Str(s1), v2) => Str(s1 + toStr(v2))
        case (v1, Str(s2)) => Str(toStr(v1) + s2)
        case (v1, v2) => Num(toNum(v1) + toNum(v2))
      
      case BinOp(Minus, e1, e2) => Num(eToNum(e1) - eToNum(e2))
      
      case BinOp(Times, e1, e2) => Num(eToNum(e1) * eToNum(e2))
      
      case BinOp(Div, e1, e2) => Num(eToNum(e1) / eToNum(e2))
      
      case BinOp(bop @ (Eq | Ne), e1, e2) => 
        val v1 = eToVal(e1)
        val v2 = eToVal(e2)
        bop match {
          case Eq => Bool (v1 == v2)
          case _ => Bool (v1 != v2)
        }
        
      case BinOp(bop @ (Lt | Le | Gt | Ge), e1, e2) => 
        Bool(inequalityVal(bop, eToVal(e1), eToVal(e2)))
      
      case BinOp(And, e1, e2) => 
        val v1: Val = eToVal(e1)
        if toBool(v1) then eToVal(e2) else v1
        
      case BinOp(Or, e1, e2) =>
        val v1: Val = eToVal(e1)
        if toBool(v1) then v1 else eToVal(e2)
      
      case BinOp(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      
      case If(e1, e2, e3) => if eToBool(e1) then eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, ed, eb) => eval(extend(env, x, eToVal(ed)), eb)
  
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Val = eval(emp, parse.fromString(s))


  /* Interface to run your interpreter from the command line.  You can ignore the code below. */ 
  
  def processFile(file: java.io.File): Unit =
    if debug then
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    
    val expr = handle(fail()) {
      parse.fromFile(file)
    }
      
    if debug then
      println("Parsed expression:")
      println(expr)

    
    handle(()) {
      val v = eval(expr)
      println(v.prettyVal)
    }
