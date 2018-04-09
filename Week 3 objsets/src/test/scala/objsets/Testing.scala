package objsets

class Poly (val terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0

  def this(args: (Int, Double)*) = this(args.toMap)

  def +(other: Poly) = new Poly(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String = {
    (for ((exp, coeff) <- terms.toList.sorted(Ordering[(Int, Double)].reverse))
      yield coeff + "x^" + exp).mkString(" + ")
  }

}

object Testing extends App {
  new Poly(Map(1 -> 2.0))
  print("Hello")
}
