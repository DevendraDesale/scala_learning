// Writing a tail-recursive functions
def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

def sumInts2(a: Int, b: Int): Int = sum(x => x, a, b)
def sumCubes2(a: Int, b: Int): Int = sum(x => x * x * x, a, b)
def sumFactorials2(a: Int, b: Int) = sum(fact, a, b)

def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)


// To avoid repetition lets rewrite the sum function

def sum3(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

// so now we return the sum directly
def sumInts5 = sum3(x => x)
def sumCubes5 = sum3(x => x * x * x)
def sumFacts5 = sum3(fact)

sumCubes5(1, 10) + sumFacts5(1, 3)

// Writing same function with different formatting
sum3(x => x * x * x)(1, 10)
//sum3(x => x * x * x) == sumCubes

// Scala has its special syntax for defining the functions returning functions
def sum4(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum4(f)(a + 1, b)


//Write a product function that calculates the product of the values of a function for the points on a given interval.
def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1 else f(a) * product(f)(a + 1, b)
}
product(x => x)(2, 4)

def fact2(x: Int): Int = product(x => x)(1, x)

fact2(3)
// Combining the add and product we get mapReduce function
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

def product2(f: Int => Int)(a: Int,b:Int): Int = mapReduce(f,(x,y)=> x*y,1)(a,b)

product2(x=>x*x)(3,4)