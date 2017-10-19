object eratosthenes {
  def main(args: Array[String]): Unit = {
    print("Enter the number until which you want to show prime numbers: ")
    val number = scala.io.StdIn.readInt()

    if(Sieve(number) != null)
      print(Sieve(number))
  }

  def Sieve(number: Int): List[Int] = {
    if(number < 2){
      print("Enter number greater than 2.")
      null
    } else {
      val oddList = 3 to number by 2 toList
      def primeNumbers(oddList: List[Int], prime: List[Int]): List[Int] = oddList match {
        case Nil => prime
        case _ if prime.exists(oddList.head % _ == 0) => primeNumbers(oddList.tail, prime)
        case _ => primeNumbers(oddList.tail, oddList.head :: prime)
      }
      primeNumbers(oddList, List(2)).reverse
    }
  }
}
