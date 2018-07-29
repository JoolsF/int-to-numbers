import Utilties.leftPad

object IntToWords {

  private val ones: Map[Char, String] = Map(
    '0' -> "",
    '1' -> "one",
    '2' -> "two",
    '3' -> "three",
    '4' -> "four",
    '5' -> "five",
    '6' -> "six",
    '7' -> "seven",
    '8' -> "eight",
    '9' -> "nine"
  )

  private val onesMinusZero: Map[Char, String] = ones - '0'

  private val teens: Map[Char, String] = Map(
    '0' -> "ten",
    '1' -> "eleven",
    '2' -> "twelve",
    '3' -> "thirteen",
    '4' -> "fourteen",
    '5' -> "fifteen",
    '6' -> "sixteen",
    '7' -> "seventeen",
    '8' -> "eighteen",
    '9' -> "nineteen"
  )

  private val twentyToNinety: Map[Char, String] = Map(
    '2' -> "twenty ",
    '3' -> "thirty ",
    '4' -> "forty ",
    '5' -> "fifty ",
    '6' -> "sixty ",
    '7' -> "seventy ",
    '8' -> "eighty ",
    '9' -> "ninety ",
  )

  private val onesOrTwenty: Map[Char, Map[Char, String]] = Map(
    '0' -> onesMinusZero,
    '1' -> teens
  )

  def oneToOneThousand(s: String, and: Boolean = false): Option[String] = {
    val str = leftPad(s, 3)
    val hundred: Option[String] = onesMinusZero.get(str(0)).map(_ + " hundred")
    val tens: Option[String] = onesOrTwenty.get(str(1)).fold {
      twentyToNinety.get(str(1)).map(_ + ones(str(2)))
    }(oneToTwenty => oneToTwenty.get(str(2)))

    (hundred, tens) match {
      case (Some(h), Some(t)) => Some(s"$h and $t")
      case (None, Some(t)) if and => Some(s"and $t")
      case _ => List(hundred, tens).flatten.headOption
    }
  }


  def oneToOneThousandWithSuffix(number: String, suffix: String, and: Boolean = false): Option[String] =
    oneToOneThousand(number, and)
      .fold {
        Option.empty[String]
      }(number => Some(s"$number $suffix"))


  def toWordsSequence(i: Int): Seq[Option[String]] = {
    val number = i.toString
    val range = 0 until number.length reverse

    def index(i: Int) = (i, number.length) match {
      //Millions
      case (8, 9) => oneToOneThousandWithSuffix(number.substring(0, 3), "million")
      case (7, 8) => oneToOneThousandWithSuffix(number.substring(0, 2), "million")
      case (6, 7) => oneToOneThousandWithSuffix(number.substring(0, 1), "million")
      //Thousands
      case (5, n) if n >= 6 => oneToOneThousandWithSuffix(number.substring(n - 6, (n - 6) + 3), "thousand")
      case (4, 5) => oneToOneThousandWithSuffix(number.substring(0, 2), "thousand")
      case (3, 4) => oneToOneThousandWithSuffix(number.substring(0, 1), "thousand")
      // Hundreds
      case (2, n) if n >= 3 => oneToOneThousand(number.substring(n - 3, (n - 3) + 3), true)
      case (1, 2) => oneToOneThousand(number)
      case (0, 1) => oneToOneThousand(number)
      case _ => None
    }

    range map index
  }

  def toWords(i: Int): String = toWordsSequence(i).flatten mkString ", "
}