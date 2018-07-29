import org.scalatest.{FlatSpec, Matchers}

class IntToWordsTest extends FlatSpec with Matchers {

  import IntToWords._

  it should "convert an integer to words for values less than 10" in {
    toWords(0) shouldBe ""
    toWords(1) shouldBe "one"
    toWords(2) shouldBe "two"
    toWords(3) shouldBe "three"
    toWords(4) shouldBe "four"
    toWords(5) shouldBe "five"
    toWords(6) shouldBe "six"
    toWords(7) shouldBe "seven"
    toWords(8) shouldBe "eight"
    toWords(9) shouldBe "nine"
  }

  it should "convert an integer to words for values between than 10 and 19" in {
    toWords(10) shouldBe "ten"
    toWords(11) shouldBe "eleven"
    toWords(12) shouldBe "twelve"
    toWords(13) shouldBe "thirteen"
    toWords(14) shouldBe "fourteen"
    toWords(15) shouldBe "fifteen"
    toWords(16) shouldBe "sixteen"
    toWords(17) shouldBe "seventeen"
    toWords(18) shouldBe "eighteen"
    toWords(19) shouldBe "nineteen"
  }
  it should "convert an integer to words for values between than 20 and 99" in {
    toWords(20) shouldBe "twenty "
    toWords(21) shouldBe "twenty one"
    toWords(40) shouldBe "forty "
    toWords(41) shouldBe "forty one"
    toWords(60) shouldBe "sixty "
    toWords(61) shouldBe "sixty one"
    toWords(80) shouldBe "eighty "
    toWords(81) shouldBe "eighty one"
    toWords(99) shouldBe "ninety nine"
  }
  it should "convert an integer to words for values between than 100 and 999" in {
    toWords(100) shouldBe "one hundred"
    toWords(101) shouldBe "one hundred and one"
    toWords(199) shouldBe "one hundred and ninety nine"
  }

  it should "convert an integer to words for values between than 1000 and 999999" in {
    toWords(1000) shouldBe "one thousand"
    toWords(1100) shouldBe "one thousand, one hundred"
    toWords(1101) shouldBe "one thousand, one hundred and one"
    toWords(300550) shouldBe "three hundred thousand, five hundred and fifty "
    toWords(999999) shouldBe "nine hundred and ninety nine thousand, nine hundred and ninety nine"
  }

  it should "convert an integer to words for values between 1 million and 1 billion" in {
    toWords(5000000) shouldBe "five million"
    toWords(5001101) shouldBe "five million, one thousand, one hundred and one"
    toWords(2000400) shouldBe "two million, four hundred"
    toWords(999999999) shouldBe "nine hundred and ninety nine million, nine hundred and ninety nine thousand, nine hundred and ninety nine"
  }

}
