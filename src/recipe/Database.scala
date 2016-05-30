package recipe

abstract class Database extends FoodCategories {
  def allFoods: List[Food]
  def allRecipes: List[Recipe]
  def allCategories: List[FoodCategory]

  def findFood(name: String): Option[Food] = allFoods.find(_.name == name)
}
