
val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")


def go(list : List[String]) : Unit = {
  val intList : List[Int] = goHelper(list)

  println("Translation: " + printList(intList, " "))
  println("Addition: " + printList(intList, " + ") + " = " + goAddition(intList) + " ")
  println("Multiplication: " + printList(intList, " * ") + " = " + goMultiply(intList) + " ")
}

def goAddition(list : List[Int]) : Int = {
  list.foldLeft(0)(_ + _)
}

def goMultiply(list : List[Int]) : Int = {
  list.foldLeft(1)(_ * _)
}

def goHelper(list : List[String]) : List[Int] = {
  list match {
    case Nil => Nil
    case head::tail =>
      chinese.contains(head) match {
        case true => chinese.indexOf(head)::goHelper(tail)
        case false =>
          english.contains(head) match {
            case true => english.indexOf(head)::goHelper(tail)
            case false => Nil
          }
      }
  }
}

def printList(list : List[Int], s : String) : String = {
  list match {
    case Nil => ""
    case head::tail =>
      tail match {
        case Nil => head + ""
        case _ => head + s + printList(tail, s)
      }
  }
}

go(List("yi", "nine", "six", "ba", "josh"))

