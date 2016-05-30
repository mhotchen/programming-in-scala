package recipe

trait SimpleRecipes {
  this: SimpleFoods =>

  object FruitSalad extends Recipe("Fruit salad", List(Apple, Pear), "Mix it all together")

  def allRecipes = List(FruitSalad)
}
