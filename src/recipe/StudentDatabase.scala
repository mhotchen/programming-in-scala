package recipe

object StudentDatabase extends Database {
  object FrozenChips extends Food("Frozen chips")
  object HeatItUp extends Recipe("Heat it up", List(FrozenChips), "Microwave the food for 2 minutes.")

  def allFoods = List(FrozenChips)
  def allRecipes = List(HeatItUp)
  def allCategories = List(FoodCategory("easy", List(FrozenChips)))
}
