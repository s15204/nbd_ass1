import scala.util.control.Breaks.break

class Main {
  def printList (list: List[String]): String = {
    var output: String = ""
    for (day <- list)
      output += day.concat(", ")
    return output.substring(0, output.length - 2)
  }

  def printListCond (list: List[String]): String = {
    var output = ""
    for (day <- list if day.startsWith("S"))
      output += day.concat(", ")
    return output.substring(0, output.length - 2)
  }

  def printListWhile (list: List[String]): String = {
    var i = 0
    var output = ""
    while (true) {
      output += list(i)
      i += 1
      if (i == list.length) {
        return output
      }
      print(", ")
    }
    return output
  }


}


object Hello extends App {
  val main: Main = new Main

  // 0
  println("Hello, World!")

  val days: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  // 1a
  println(main.printList(days))
  // 1b
  println(main.printListCond(days))
  // 1c
  println(main.printListWhile(days))

  
}
