println("hello")

val i = 1
i + 3

def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a + 1, b)
sumInts(2, 4)


def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

sumCubes(2, 4)

def sumFactorials(a: Int, b: Int): Int =
  if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)
// Generalizing the higher order concepts
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a, b)

//rewritting the functions again.

def sumInts2(a: Int, b: Int): Int = sum(id, a, b)
def sumCubes2(a: Int, b: Int): Int = sum(cube, a, b)
def sumFactorials2(a: Int, b: Int) = sum(fact, a, b)

def id(x: Int): Int = x
def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)
def cube(a: Int): Int = a * a * a

// Using anonymous functions to simplify the langauge construct
def sumInts3(a: Int, b: Int): Int = sum(x => x, a, b)
def sumCubes3(a: Int, b: Int): Int = sum(x => x * x * x, a, b)

// Writing a tail-recursive functions
def sum2(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}
def sumInt4(a: Int, b: Int): Int = sum2(x=>x)(a, b)
sumInt4(2, 4)

