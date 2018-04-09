val mnem = Map('2' -> "ABC", '3' -> "DEF")

"abc"

val invert = for {
  (k, v) <- mnem
  c <- v
} yield (c, k)


List(1,2,3)


val test = "DAFA"


test map invert
//test.flatMap(invert(x).toString)