import org.scalatest.{FlatSpec, Matchers}

class ForComprehensionsTest extends FlatSpec with Matchers {
  /*
  * for (i <- array) yield j
  * translates to:
  * array.map(i => j)
  *
  *
  * for (i <- array if (i % 2 == 0)) yield i * 2
  * translates to:
  * array withFilter (i % 2 == 0) map (i => i * 2)
  *
  *
  * for (i <- array; j <- Array(i, i + 1) yield j
  * translates to:
  * array.flatMap(i => Array(i, i + 1))
  * */

  case class Book(title: String, isbn: String, authors: List[String])

  val books = List(
    Book("foo", "1", List("a1", "a2")),
    Book("bar", "2", List("a3")),
    Book("baz", "3", List("a1", "a2")),
    Book("abc", "4", List("a2", "a3", "a4")),
    Book("def", "5", List("a1"))
  )

  "transform" should "filter books by title" in {
    assertResult(Book("baz", "3", List("a1", "a2")))((for (b <- books if b.title == "baz") yield b).head)
    assertResult(Book("baz", "3", List("a1", "a2")))(books.filter(b => b.title == "baz").head)
  }

  it should "list names of all authors" in {
    assertResult(Set("a1", "a2", "a3", "a4"))((for (b <- books; a <- b.authors) yield a).toSet)
    assertResult(Set("a1", "a2", "a3", "a4"))(books.flatMap(b => b.authors).toSet)
  }

  it should "list all book titles" in {
    assertResult(List("foo", "bar", "baz", "abc", "def"))(for (b <- books) yield b.title)
    assertResult(List("foo", "bar", "baz", "abc", "def"))(books.map(b => b.title))
  }

  it should "list all authors with a count of books authored by them" in {
    val resultUsingFor = (for (b <- books; a <- b.authors) yield (a, 1)).groupBy(_._1).map { case (k, v) => (k, v.size) }
    val result = books.flatMap(b => b.authors.map(a => (a, 1))).groupBy(_._1).map { case (k, v) => (k, v.size) }

    assertResult(Map("a1" -> 3, "a3" -> 2, "a2" -> 3, "a4" -> 1))(resultUsingFor)
    assertResult(Map("a1" -> 3, "a3" -> 2, "a2" -> 3, "a4" -> 1))(result)
  }

  it should "convert list of i to list i and i * i" in {
    val result = (0 to 3).flatMap(i => Array(i, i * i))
    val resultUsingFor = for (i <- 0 to 3; j <- Array(i, i * i)) yield j

    assertResult(List(0, 0, 1, 1, 2, 4, 3, 9))(result)
    assertResult(List(0, 0, 1, 1, 2, 4, 3, 9))(resultUsingFor)
  }

  it should "convert list of i to list i and i * i only when i is even" in {
    val result = (0 to 4).withFilter(isEven).flatMap(i => Array(i, i * i))
    val resultUsingFor = for (i <- 0 to 4; j <- Array(i, i * i) if isEven(i)) yield j

    assertResult(List(0, 0, 2, 4, 4, 16))(result)
    assertResult(List(0, 0, 2, 4, 4, 16))(resultUsingFor)
  }

  private def isEven(i: Int): Boolean = i % 2 == 0
}
