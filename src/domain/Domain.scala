package domain

object Domain {
  def apply(parts: String*): String = parts.reverse.mkString(".")

  def unapplySeq(whole: String): Option[Seq[String]] =
    Some(whole.split('.').reverse)
}
