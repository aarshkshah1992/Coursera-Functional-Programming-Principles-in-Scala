package forcomp

object Test extends App {

  import forcomp.Anagrams.{Occurrences, _}

  def wordOccurrences(w: Word): Occurrences = (w.toLowerCase groupBy identity).map {
    case (c, allC) => (c, allC.length)
  }.toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.foldLeft("")(_ + _))

  sentenceOccurrences(List("My", "name", "is", "aarsh"))


  dictionary.groupBy(wordOccurrences(_)).head

  def combinationss(occurrences: Occurrences): List[Occurrences] = {
    print(occurrences + "\n")
    if (occurrences.isEmpty) List(List())
    else {
      for {
        rest <- combinationss(occurrences.tail)
        (c,freq) = occurrences.head
        i <- 0 to freq
      } yield if (i == 0) rest else (c, i) :: rest
    }
  }

  val x = combinationss(wordOccurrences("abab"))

  print("\n Answers is \n")
  print(x.mkString("\n"))


}