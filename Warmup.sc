
def prime(n : Int) : Boolean = {
  primeHelper(n, 2)
}

def primeHelper (n : Int, index : Int) : Boolean = {
  (index >= n) match {
    case true => true
    case false =>
      (n % index == 0) match {
      case true => false
      case false => primeHelper(n, index+1)
    }
  }
}

def twinprimes(n : Int, m : Int) : Boolean = {
  (prime(n) && prime(m)) && (n-m == 2 || n-m == -2)
}


def twinprimeslist(n : Int) : List[Int] = {
  twinprimelistHelper(n, 3).distinct
}

def twinprimelistHelper(n : Int, index : Int) : List[Int] = {
  (index >= n) match {
    case true => Nil
    case false =>
      twinprimes(index, index+2) match {
        case false => twinprimelistHelper(n, index+1)
        case true =>
          index::(index+2)::twinprimelistHelper(n, index+1)
      }
  }
}

def goldbach(n : Int) : Unit = {
  ((n >= 2) && (n % 2 == 0)) match {
    case false => Nil
    case true =>
      goldbachHelper(n, n-2) match {
        case Nil =>
        case head :: tail => println(head + " + " + tail.head + " = " + n + " ")
      }
  }
}

def goldbachHelper(n : Int, index : Int) : List[Int] = {
  (index >= 1) match {
    case false => Nil
    case true =>
      (prime(index) && index <= n) match {
        case false => goldbachHelper(n, index - 1)
        case true => index::goldbachHelper(n-index, n-index)
      }
  }
}

println("Prime test: ")
println("17: " + prime(17) + " ")
println("16: " + prime(16) + " ")

println("Twinprime test: ")
println("17 and 19: " + twinprimes(17,19) + " ")
println("17 and 13: " + twinprimes(17,13) + " ")

println("Twinprime list test: ")
println("List to 50: " + twinprimeslist(50) + " ")

println("Goldbach test: ")
println("28: ")
println(goldbach(28) + " ")
println("27: ")
println(goldbach(27) + " ")
println("4: ")
println(goldbach(4) + " ")



