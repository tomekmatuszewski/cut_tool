
class CsvHandler(val delimiter: String , val path: String, val fields: String) {
  val fieldsList = fields.split(",").toList
  def read: String = {
    val lines = readFile(path).map(_.split(delimiter))
    if (fields != "") {
      val filteredLines = lines.map {
        case line => {
          val linewithIndex = line.zipWithIndex.map((x,y) => (x,y + 1))
          val filteredLine = linewithIndex.filter {
            case (_, index) => fieldsList.contains(index.toString)
          }
          filteredLine.map(_._1)
        }
      }
      filteredLines.map(_.mkString(",")).mkString("\n")
    } else {
      lines.map(_.mkString(",")).mkString("\n")
  }
}
  def readFile(path: String): Iterator[String] = {
    if (path == "-") {
      scala.io.Source.stdin.getLines()
    } else {
      scala.io.Source.fromFile(path).getLines()
    }
  }
}
@main def hello(args: String*): Unit =
  val usage = """
  Usage: sliding [--f filename] Optional [--d delimiter]
  """
  if (args.isEmpty || args.length % 2 != 0) {
    println(usage)
    System.exit(1)
  }

  val argMap = Map.newBuilder[String, Any]
  args.sliding(2, 2).toList.collect {
    case Seq(s"--f${fields}", arg1: String) => {
      argMap.+=("file" -> arg1)
      argMap.+=("fields" -> fields)
    }
    case Seq("--d", arg2: String) => argMap.+=("delimiter" -> arg2)
    case _ => {
      println(usage)
      System.exit(1)
    }
  }
  val parsedArgs = argMap.result()
  
  val delimiter = parsedArgs.getOrElse("delimiter", "\t").toString()
  val lines = new CsvHandler(
    delimiter,
    parsedArgs("file").toString,
    parsedArgs("fields").toString
  ).read
  println(lines)


