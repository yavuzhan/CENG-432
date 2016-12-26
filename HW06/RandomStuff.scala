trait RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int]
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean
  def executeWithRetry(retryCount: Int, op: => Int): Option[Int]
}
object RandomStuff extends RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int] = {
    val result = for (index <- list) yield op(index)
    result
  }

  def allValid(list: List[Int], op: (Int) => Boolean): Boolean = {
    var flag = true
    list.foreach((index: Int) => {
      if (!op(index))
        flag = false
    })
    flag
  }

  def executeWithRetry(retryCount: Int, op: => Int): Option[Int] = {
    if (retryCount != 0) {
      try {
        Some(op)
      } catch {
        case e: Exception =>
          executeWithRetry(retryCount - 1, op)
      }
    } else
      None
  }
}
