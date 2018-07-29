import scala.util.Try

object Main extends App {

  args.headOption match {
    case Some(i) =>
      Try(i.toInt).toEither.fold(_ =>
        println("Input not an integer"),
        number => println(IntToWords.toWords(number))
      )
    case _ => println("Integer argument required.")
  }


}
