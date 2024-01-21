import java.io.PrintWriter
import scala.util.Using

case class MainGame(games: List[Game])



object MainGame {

  def main(args: Array[String]): Unit = {

    val mainGame = new MainGame()
    Using(scala.io.Source.fromFile("input.txt")) {
     mainGame = minGame = f => transform(f.getLines())
    }.getOrElse(0)
    sumOfGamesIds()



  }



  def transform(list: Iterator[String]): MainGame = {
      val games = list.flatMap( line =>
        val parts = line.split(":")
        if (parts.length == 2){
          val gameId = parts(0).filter(_.isDigit).toInt
          val cubeSetList = parts(1).split(";").toList.flatMap(set =>
            /*
            * Original String: " 3 blue, 4 red "
            trim will remove white spaces from the srting that are soorunding it
            result "3 blue, 4 red"
            * */
            val colors = set.split(",").map(_.trim)
            val (red, blue, green) = colors.foldLeft((0,0,0))((acc, colorCount) => {
              val Array(count, color) = colorCount.split(" ")
              val countInt = count.toInt
              color match
                case "red" => (acc._1 + countInt, acc._2, acc._3)
                case "green" => (acc._1, acc._2 + countInt, acc._3)
                case "blue" => (acc._1 , acc._2, acc._3 + countInt)
                case _ => acc
            })
            List(CubeSet(red, blue, green))

          )
          Some(Game(gameId, cubeSetList))
        } else
          None
      ).toList
      MainGame(games)


    }



  def sumOfGamesIds(mainGame: MainGame, green: Int, red: Int, blue: Int, filename: String): Int =
    val writer = new PrintWriter(new java.io.File(filename))
    try {
      for (game <- mainGame.games if game.turns.forall(cubeset => game.chceckRule(cubeset, green, red, blue)))
        writer.println(game)
    }
    finally {
      writer.close()
    }

    val sumOfTheIds =
      for
        game <- mainGame.games
        if game.turns.forall(cubeset => game.chceckRule(cubeset, green, red, blue))
      yield game.id
    sumOfTheIds.sum


//method for practice, not used for this example
  def checkTheGames(mainGame: MainGame, green: Int, red: Int, blue: Int): IndexedSeq[Game] =
    (for
      game <- mainGame.games
      if game.turns.forall(cubeset => game.chceckRule(cubeset, green, red, blue))
     yield game).toIndexedSeq

  def writeToFile( filename: String): Unit =
    val writer = new PrintWriter(new java.io.File(filename))
    try {

    }
    finally {
      writer.close()
    }


}
