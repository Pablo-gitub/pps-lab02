package ExploringAutonomously

object Lab2 extends App:

  // standard function with no currying
  def mult(x: Double, y: Double): Double = x * y

  // function with currying
  // curriedMult has actually type: Double => (Double => Double)
  def curriedMult(x: Double)(y: Double): Double = x * y

  println(mult(3,4)) //Expected value = 3*4 = 12
  println(curriedMult(3)(4)) //Expected value = 3*4 = 12

  // Define a curried function that multiplies three Double values.
  private def curriedTrice(x: Double)(y: Double)(z: Double): Double = x * y * z

  // Fully apply the function with all three arguments;
  private val tri = curriedTrice(2)(3)(4)

  // Partially apply the function with the first argument (4);
  // this returns a function that takes two more Double arguments.
  private val tris = curriedTrice(4)

  // Apply the partially applied function with a second argument (4);
  // this returns a function that takes the third argument.
  private val trick: Double => Double = tris(4)
  //Print the result
  println(tri) //Expected value = 2*3*4 = 24
  println(tris(2)(5)) //Expected value = 4*2*5 = 40
  println(trick(5)) //Expected value = 4*4*5 = 80

  private def divideCurried(x: Double)(y: Double): Double = y/x

  private val divideByThree = divideCurried(3)
  println(divideByThree(9)) //Expected value 9/3 = 3
  println(divideByThree(3)) //Expected value 3/3 = 1

  private val positiveLambda: Int => String = (n: Int) => if(n>=0) "Positive" else "Negative"

  println(positiveLambda(6))
  println(positiveLambda(-2))

  private def positive(n: Int): String = n match
    case n if n >= 0 => "Positive"
    case n if n < 0 => "Negative"

  println(positive(4))
  println(positive(-3))
  println(positive(0))

  val negLambda: (String => Boolean) => (String => Boolean) = predicate => s => !predicate(s)

  def neg(predicate: String => Boolean): String => Boolean = s => !predicate(s)


  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmptyLambda = negLambda(empty) // which type of notEmpty?
  println(notEmptyLambda("foo")) // true
  println(notEmptyLambda("")) // false
  println(notEmptyLambda("foo") && !notEmptyLambda("")) // true.. a comprehensive test

  private val notEmpty = neg(empty) // which type of notEmpty?
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

  private def negGeneric[X](predicate: X => Boolean): X => Boolean = x => !predicate(x)

  private val isEmptyGeneric: String => Boolean = _.isEmpty
  val notEmptyGeneric: String => Boolean = negGeneric(isEmptyGeneric)
  println(isEmptyGeneric("foo")) //true
  println(notEmpty("")) //false

  private val isEven: Int => Boolean = n => n % 2 == 0
  val isOdd: Int => Boolean = negGeneric(isEven)
  println(isOdd(3)) //true
  println(isOdd(2)) //false

  val p1: Int =>Int=>Int=>Boolean = (x:Int) => (y:Int) => (z: Int) => x<=y && y==z
  val p2: (Int,Int,Int)=>Boolean = (x,y,z) => x<=y && y==z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x<=y && y==z
  def p4(x:Int, y:Int, z:Int):Boolean = x<=y && y==z

  // Using the curried function value p1
  println("curried function value p1")
  println(p1(1)(2)(2)) // Expected: true, because 1 <= 2 and 2 == 2
  println(p1(3)(2)(2)) // Expected: false, because 3 <= 2 is false

  // Using the non-curried function value p2
  println("non-curried function value p2")
  println(p2(1, 2, 2)) // Expected: true
  println(p2(3, 2, 2)) // Expected: false

  // Using the curried method p3
  println("curried method p3")
  println(p3(1)(2)(2)) // Expected: true
  println(p3(3)(2)(2)) // Expected: false

  // Using the non-curried method p4
  println("non-curried method p4")
  println(p4(1, 2, 2)) // Expected: true
  println(p4(3, 2, 2)) // Expected: false

  private def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  println(compose(_ - 1, _ * 2)(5)) // 9

  val f: Double=>Double = (x: Double) => x-1
  private def composeGeneric[X](f: X => X, g: X => X): X => X = x => f(g(x))
  //The only constraint imposed here is that both functions must be endofunctions
  // on the same type X—that is, they both have the same domain and codomain (X).
  println(composeGeneric(f(_), _ * 2)(5)) // 9.0

  val g: Double => String = x => if(x>=0) "Positive" else "Negative"
  private def composeMoreGeneric[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))
  // The only constraint imposed here is that the codomain of g (B) must equal the domain of f (B).
  println(composeMoreGeneric(g(_),f(_))(5)) //Positive
  println(composeMoreGeneric(g(_),f(_))(0)) // Negative

  private def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = x => f(g(h(x)))
  println(composeThree(
    (s: String) => s + "!",
    (i: Int)    => i.toString,
    (a: Int)    => a * 2
  )(3)) //6!

  private def composeThreeWithCompose[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = composeMoreGeneric(f, composeMoreGeneric(g,h))
  println(composeThreeWithCompose(
    (s: String) => s + "!",
    (i: Int) => i.toString,
    (a: Int) => a * 2
  )(3)) //6!

  private def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case exponent if exponent > 0 => base * power(base, exponent -1)
    //case exponent if exponent < 0 => power(base, exponent +1) / base

  private def power2(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(exponent: Int, acc: Double): Double = exponent match
      case 0 => acc
      case exponent if(exponent>0) => _power(exponent-1, acc*base)
      //case exponent if exponent < 0 => _power(exponent + 1, acc / base)
    _power(exponent,1)

  println(power(3,3))
  //println(power(3,-3))
  println("tail")
  println(power2(3,3))
  //println(power2(3,-3))

  private def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverseNumber(n: Int, acc: Int): Int = n match
      case 0 => acc
      case _ => _reverseNumber(n/10, acc*10+n%10)
    _reverseNumber(n,0) // Start with the entire number and an accumulator of 0

  println(reverseNumber(234)) //432
  println(reverseNumber(12345)) //54321

  enum Expr:
    private case Literal(value: Int) // a numeric constant
    private case Add(first: Expr, second: Expr) // addition of two expressions
    private case Multiply(first: Expr, second: Expr) // multiplication of two expressions

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Literal(value) => value
      case Add(first, second) => evaluate(first) + evaluate(second)
      case Multiply(first, second) => evaluate(first) * evaluate(second)

    def show(expr: Expr): String = expr match
      case Literal(value) => value.toString
      case Add(first, second) => "(" + show(first) + " + " + show(second) + ")"
      case Multiply(first, second) => "(" + show(first) + " " + show(second) + ")"

    

