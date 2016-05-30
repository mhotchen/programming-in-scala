package recipe

object GotApples extends App {
  val db: Database = if(args(0) == "student") StudentDatabase else SimpleDatabase

  object browser extends Browser {val database = db}

  val apple = SimpleDatabase.findFood("Apple").get
  for (recipe <- browser.recipesUsing(apple)) println(recipe)
}
