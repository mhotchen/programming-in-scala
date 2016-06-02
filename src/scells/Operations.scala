package scells

trait Arithmetic { this: Evaluator =>
  operations += (
    "add"  -> { case List(x: Double, y: Double) => x + y },
    "sub"  -> { case List(x: Double, y: Double) => x - y },
    "div"  -> { case List(x: Double, y: Double) => x / y },
    "mul"  -> { case List(x: Double, y: Double) => x * y },
    "mod"  -> { case List(x: Double, y: Double) => x % y },
    "sum"  -> { xs: List[Double] => xs.sum },
    "prod" -> { xs: List[Double] => xs.product }
  )
}

trait Function { this: Evaluator =>
  operations += (
    "avg" -> { xs: List[Double] => xs.sum / xs.length },
    "max" -> { xs: List[Double] => xs.max },
    "min" -> { xs: List[Double] => xs.min }
  )
}
