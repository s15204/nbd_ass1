import scala.annotation.tailrec

class Main {
  def stringify (list: List[String]): String = {
    var output: String = ""
    for (day <- list)
      output += day.concat(", ")
    output.substring(0, output.length - 2)
  }

  def stringifyCond (list: List[String]): String = {
    var output = ""
    for (day <- list if day.startsWith("S"))
      output += day.concat(", ")
    output.substring(0, output.length - 2)
  }

  def stringifyWhile (list: List[String]): String = {
    var i = 0
    var output = ""
    while (true) {
      output += list(i)
      i += 1
      if (i == list.length) {
        return output
      }
      output += ", "
    }
    output
  }

  def stringifyRecurs (list: List[String]): String = {
    var output = ""
    var tail = ""

    output += list.head

    if (list.size > 1)
      tail = stringifyRecurs(list.tail)

    if (!tail.equals(""))
      output += ", "

    output += tail
    output
  }

  def stringifyRecursReverse (list: List[String]): String = {
    var output = ""
    var tail = ""

    if (list.size > 1)
      tail = stringifyRecursReverse(list.tail)

    output += tail

    if (!tail.equals(""))
      output += ", "

    output += list.head
    output
  }

  @tailrec
  final def stringifyRecursTail(list: List[String], str: String): String = {
    var output = str
    output += list.head
    if (list.tail.nonEmpty) output += ", "
    if (list.size > 1) stringifyRecursTail(list.tail, output) else output
  }

  def stringifyFL(list: List[String]): String = {
    val output = list.foldLeft("")(_ + _ + ", ")
    output.substring(0, output.length - 2)
  }

  def stringifyFR(list: List[String]): String = {
    val output = list.foldRight("")(_ + ", " + _)
    output.substring(0, output.length - 2)
  }

  def stringifyFLS(list: List[String]): String = {
    list.foldLeft("")((prev, next) => {
      if (next.startsWith("S")) {
        if (!prev.equals(""))
          prev + ", " + next
        else
          next
      } else prev
    })
  }

  def increment(list: List[Int]): List[Int] = list.map(i => i+1)

  def normalize(list: List[Double]): List[Double] = list.filter(n => -5 <= n && n <= 12).map(n => if (n < 0) n * -1 else n)

  def printTuple(tuple: (String, Int, Double)) = println(tuple._1 + ", " + tuple._2 + ", " + tuple._3)

  def recursionList(list: List[Double]): List[Double] = {
    if (list.size > 1)
    if (list.head != 0) List(list.head)++recursionList(list.tail) else recursionList(list.tail)
    else List(list.head)
  }

  def show(opt: Option[String]) = opt match {
    case Some(str) => str
    case None => "?"
  }

  def skipNone(coll:List[Option[Any]]): List[Any] = {
    coll.filter(opt => opt.isDefined).map(opt => opt.get)
  }
}


object Hello extends App {
  val main: Main = new Main

  // 0
  println("Hello, World!")

  val days: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  println("<--  1a  -->")
  println(main.stringify(days))
  println("<--  1b  -->")
  println(main.stringifyCond(days))
  println("<--  1c  -->")
  println(main.stringifyWhile(days))

  println()

  println("<--  2a  -->")
  println(main.stringifyRecurs(days))
  println("<--  2b  -->")
  println(main.stringifyRecursReverse(days))

  println()

  println("<--  3   -->")
  println(main.stringifyRecursTail(days, ""))

  println()

  println("<--  4a  -->")
  println(main.stringifyFL(days))
  println("<--  4b  -->")
  println(main.stringifyFR(days))
  println("<--  4c  -->")
  println(main.stringifyFLS(days))

  println()

  var gpus:Map[String, Double] = Map("RTX 3090" -> 1499.99, "RX 6900 XT" -> 999.99, "RTX 3080" -> 699.99, "RX 6800 XT" -> 649.99)
  println("<--  5   -->")
  println(gpus)
  gpus = gpus.map(x => (x._1, (math floor ((x._2 * 0.9) * 100))/100))
  println(gpus)

  println()

  println("<--  6   -->")
  println(main.increment(List(1, 2, 3)))

  println()

  println("<--  7   -->")
  println(main.normalize(List(1.2, -3.2, 87, 7, -4)))

  println()

  println("<--  8   -->")
  main.printTuple("🤔", 32, 1)

  println()

  println("<--  9   -->")
  println(main.recursionList(List(0, 10, 0, 2, 0, 0, 3, 1.2, 2.5)))

  println()

  var gpusByCompany:Map[String, String] = Map("AMD" -> "RX 6900 XT, RX 6800 XT, RX 6800", "Nvidia" -> "RTX 3090, RTX 3080, RTX 3070")
  println("<--  10  -->")
  println("AMD dedicated GPUs: "+main.show(gpusByCompany.get("AMD")))
  println("Intel dedicated GPUs: "+main.show(gpusByCompany.get("Intel")))

  println(main.skipNone(List(Some("first element"), None, Some(123), Some("FOUR!"), None)))
}
