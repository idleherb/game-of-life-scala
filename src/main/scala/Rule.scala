case class Rule(val birth: Seq[Int], val survive: Seq[Int]) {}

object Rule {
  def parse(rule: String): Option[Rule] = {
    if (!validate(rule)) None else parseValidatedRule(rule)
  }

  private def validate(rule: String): Boolean = {
    rule matches "^B\\d*/S\\d*$"
  }

  private def parseValidatedRule(rule: String):  Option[Rule] = {
    val matches = rule split "/"
    Some(Rule(
      birth = charsToInts(matches(0)),
      survive = charsToInts(matches(1))
    ))
  }

  private def charsToInts(chars: String): Seq[Int] = {
    chars
      .substring(1)
      .split("")
      .filter(_.length > 0)
      .map(_.toInt)
  }
}
