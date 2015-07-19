package version

import scala.util.matching.Regex

/**
 * Created by leon on 15-7-13.
 */
object VersionCheck {
  def latest(vers: Seq[String]): String = {
    vers.maxBy(((r: Regex, str: String) => str match {
      case r(v1, v2, v3, rc) => (v1.toInt, v2.toInt, Option(v3).getOrElse("0").toInt, Option(rc).getOrElse(Int.MaxValue.toString).toInt)
      case _ => throw new Exception("parse error")
    })( """(\d+)\.(\d+)(?:\.(\d+))?(?:\-RC(\d+))?""".r, _))
  }
}