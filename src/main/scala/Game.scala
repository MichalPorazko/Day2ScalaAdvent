case class Game(id: Int, turns: List[CubeSet] ) {

  def chceckRule(cubeSet: CubeSet, green: Int, red: Int, blue: Int): Boolean =
    cubeSet.blue <= blue && cubeSet.green <= green && cubeSet.red <= red

  def addCubeSet(cubeSet: CubeSet): Game =
    this.copy(turns = turns :+ cubeSet)
}

/*
* Adding Methods to Case Classes

Simplicity: Case classes are generally used for immutable data structures and are
* not intended to contain complex business logic.
* However, adding utility methods that relate directly to the data the
* case class represents is common and acceptable.

* Complexity: There's no strict limitation on the complexity or number of methods in a case class,
* but it's a good practice to keep classes focused on their primary responsibility.
* If a case class starts to have complex logic or numerous methods,
* it might be a sign to refactor some of that functionality into separate classes or objects.
* */


