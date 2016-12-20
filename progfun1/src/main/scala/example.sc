"Hello World!"


def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @annotation.tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b ) acc
    else loop(a+1, acc+f(a))
  }
  loop(a, 0)
}

sum((x) => x*x*x)(1,2)



def sum(xs: List[Int]): Int = {
  if(xs.isEmpty) return 0;

  return xs.head + sum(xs.tail);
}

sum(List(1, 1, 2));

def max(xs: List[Int]): Int = {
  if( xs.isEmpty ) return 0;
  @annotation.tailrec
  def max1(x: Int, xs1: List[Int]) : Int = {
    if( xs1.isEmpty ) return x;
    if( x > xs1.head) {
      return max1(x, xs1.tail);
    }
    else
      return max1(xs1.head, xs1.tail);
  }
  return max1( xs.head, xs.tail);
}

max(List(-1, -1, -2));


def pascal(c: Int, r: Int): Int = {
  if (c == 0 || c == r) 1
  else {
    pascal(c-1, r-1) + pascal(c, r-1)
  }
}

def balance(chars: List[Char]): Boolean = {
  if( chars.isEmpty ) true

  var open = 0

  def bal( c: Char ): Int = {
    if(c == '(' ) {
      open + 1
    }
    else if( c == ')'  ) {
      open - 1
    }
    else
      open
  }
  def check(c: Char, cs: List[Char]) : Int = {
    if( open < 0 ) open

    else if( cs.isEmpty ) {
      open = bal(c)
      open
    }
    else {
      open = bal(c)
      check( cs.head, cs.tail )
    }
  }
  check(chars.head, chars.tail)
  if(open == 0 ) true else false
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
balance(":-)".toList)
balance("())(".toList)
