object Utilties {
  /**
    * Takes a string s of length n and front pads it with 0s up to length l
    *
    * @param l designed length of string
    * @return String
    *
    * e.g if k == 3
    *         s       res
    *         1   ->  001
    *         12  ->  011
    *         123 ->  111
    *         123 -> 1111
    */
  def leftPad(s: String, l: Int): String =
    if (l < s.length) {
      s
    } else {
      val zeroes: String = (1 to l).map(_ => '0').mkString
      (zeroes + s).substring(s.length())
    }

}
