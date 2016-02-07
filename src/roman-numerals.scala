/*
 * Takes in an integer as the only argument and spits out the roman numeral representation.
 *
 * Example:
 *
 * scala roman-numerals.scala 1999
 * MCMXCIX
 *
 *
 * This is my first ever scala code and I suspect it's not very idiomatic.
 */

val romanNumerals = Map(
   1 ->  "I",   4 -> "IV",   5 ->  "V",   9 -> "IX",  10 ->  "X",   40 -> "XL", 50 -> "L",
  90 -> "XC", 100 ->  "C", 400 -> "CD", 500 ->  "D", 900 -> "CM", 1000 ->  "M"
)

// side-effects and mutation everywhere. I think Martin would weep.
var remaining = args(0).toInt
var numeralAndCount = List[Tuple2[Int, Int]]()

romanNumerals
  .keys
  .toList
  .sortWith((l, r) => l > r) // handle largest numerals first
  .foreach((i: Int) => {
    val count = remaining / i
    numeralAndCount = numeralAndCount :+ (i, count)
    remaining -= count * i
  })

numeralAndCount
  .filter(r => r._2 != 0)
  .foreach((result) => {
    print(romanNumerals(result._1) * result._2)
  })
