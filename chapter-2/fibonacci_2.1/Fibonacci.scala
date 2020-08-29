object Fibonacci {
  def fibonacci(n: Int): Int = 
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n - 2) + fibonacci(n - 1)

  def main(args: Array[String]): Unit = {
    println(fibonacci(args(0).toInt))
  }
}
