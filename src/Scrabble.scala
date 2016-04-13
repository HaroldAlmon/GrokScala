object Scrabble {
  case class Board(
                    grid: Vector[Vector[Option[Tile]]]
                    , players: List[Player]
                    , playerLastPlayed: Player
                  )

  def main(args: Array[ String ]): Unit = {
    println("tuples = " + tuples.toString())
    println("bag = " + bag.toString())
  }

  case class Tile(
                   letter: Char
                   , value: Int
                 )

  case class Bag(
                  letterTiles: List[Tile]
                )

  case class Player(
                     name: String
                     , rack: List[Tile]
                   )

  val tilesAsText =
  """| A	9	1
    | B	2	3
    | C	2	3
    | D	4	2
    | E	12	1
    | F	2	4
    | G	3	2
    | H	2	4
    | I	9	1
    | J	1	8
    | K	1	5
    | L	4	1
    | M	2	3
    | N	6	1
    | O	8	1
    | P	2	3
    | Q	1	10
    | R	6	1
    | S	4	1
    | T	6	1
    | U	4	1
    | V	2	4
    | W	2	4
    | X	1	8
    | Y	2	4
    | Z	1	10
    |  	2	0
""".stripMargin.lines.toList

//  tilesAsText.head

  val x: List[String] =
    tilesAsText(4).tail.map(
      char =>
        if (char.isLetterOrDigit || (char == ' '))
          char
        else
          '|'
    ).split( """\|""").toList

  val tuple3: (Char, Int, Int) =
    x match {
      case List(letter, count, value) => (letter.head, count.toInt, value.toInt)
    }

  def lineToTuple(line: String): (Char, Int, Int) =
    line.tail.map(
      char =>
        if (char.isLetterOrDigit || (char == ' '))
          char
        else
          '|'
    ).split( """\|""").toList match {
      case List(letter, count, value) =>
        (letter.head, count.toInt, value.toInt)
    }

  val tuples = tilesAsText.map(lineToTuple)

  val bag = fromTuplesToBag(tuples)

  def fromTuplesToBag(tuples: List[(Char, Int, Int)]) =
    tuples.map(tuple => Tile(tuple._1, tuple._2))
}
