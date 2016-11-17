
val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")


//Retrieves the translated list and performs the necessary operations on that list
def go(list : List[String]) : Unit = {
  val intList : List[Int] = goHelper(list)

  println("Translation: " + printList(intList, " "))
  println("Addition: " + printList(intList, " + ") + " = " + goAddition(intList) + " ")
  println("Multiplication: " + printList(intList, " * ") + " = " + goMultiply(intList) + " ")
}

//Performs addition on translated list
def goAddition(list : List[Int]) : Int = {
  list.foldLeft(0)(_ + _)
}

//Performs multiplication on translated list
def goMultiply(list : List[Int]) : Int = {
  list.foldLeft(1)(_ * _)
}

//Translates the list from english and chinese to a List[Int]
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

//Prints a list with a character (' ', '+', '*') between each number
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

