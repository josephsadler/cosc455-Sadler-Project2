// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Joseph Sadler

// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  remainingBits match {
    case Nil =>
      carryBit match {
        case true => List(true)
        case false => Nil
      }
    case head::tail =>
      head match {
        case _ =>
          carryBit match {
            case true => !head::finishBinaryAdd(tail, head)
            case false => remainingBits
          }
      }
  }
}

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carry : Boolean): Boolean = {
  (pBit && qBit) || ((pBit ^ qBit) && carry)
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  ((pBit ^ qBit) ^ carryBit)
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (smallerList(pBits, qBits).isEmpty) match {
    case true => finishBinaryAdd(biggerList(pBits, qBits), carryBit)
    case false => addBits(pBits.head, qBits.head, carryBit)::doBinaryAddition(pBits.tail, qBits.tail, getNextCarryBit(pBits.head, qBits.head, carryBit))
  }
}

//Determines which list is smaller and returns it. If they are the same length, returns pList
def smallerList(pList : List[Boolean], qList : List[Boolean]) : List[Boolean] = {
  (pList.length == qList.length) match {
    case true => pList
    case false =>
      (pList.length < qList.length) match {
        case true => pList
        case false => qList
      }
  }
}

//Determines which list is larger and returns it. If they are the same length, returns pList
def biggerList(pList : List[Boolean], qList : List[Boolean]) : List[Boolean] = {
  (pList.length == qList.length) match {
    case true => pList
    case false =>
      (pList.length > qList.length) match {
        case true => pList
        case false => qList
      }
  }
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]): List[Boolean] = {
  intList match {
    case Nil => Nil
    case head::tail =>
      head match {
        case 0 => false::convertIntListToBooleanList(tail)
        case 1 => true::convertIntListToBooleanList(tail)
      }
  }
}


// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]): List[Int] = {
  booleanList match {
    case Nil => Nil
    case head::tail =>
      head match {
        case false => 0::convertBooleanListToIntList(tail)
        case true => 1::convertBooleanListToIntList(tail)
      }
  }
}



/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean
    2. Reverse the lists (since binary addition is performed right to left)
    3. Perform the binary addition with the doBinaryAddition function
    4. Reverse the lists (to get back in proper order)
    5. Convert the answer back to binary integer form for output
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]): List[Int] = {
  convertBooleanListToIntList(doBinaryAddition(convertIntListToBooleanList(pList).reverse, convertIntListToBooleanList(qList).reverse, false)).reverse
}


// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Extra Credit workspace

def binarySubtraction(pList: List[Int], qList: List[Int]): List[Int] = {
  binaryAddition(pList, toTwosComplement(qList))
}

def toTwosComplement(aList : List[Int]) : List[Int] = {
  val twos : List[Int] = List(1)
  println("heloo     " +  binaryAddition(convertBooleanListToIntList(convertIntListToBooleanList(aList).map(b => !b)), twos))
  binaryAddition(convertBooleanListToIntList(convertIntListToBooleanList(aList).map(b => !b)), twos)
}

val test1 : List[Int] = List(0, 1, 1, 1)
val test2 : List[Int] = List(0, 0, 0, 1)

binarySubtraction(test1, test2)