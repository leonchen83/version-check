package version

import scala.util.matching.Regex

/**
 * Created by leon on 15-7-13.
 */
object VersionCheck {
  def latest(vers: Seq[String]): String = {
    vers.maxBy(((r: Regex, str: String) => str match {
      case r(v1, v2, v3, rc) => (v1.toInt, v2.toInt, Option(v3) match {
        case Some(v) => v.toInt
        case None => 0
      }, Option(rc) match {
        case Some(v) => v.toInt
        case None =>
          // compare tricky
          Int.MaxValue
      })
      case _ => throw new Exception("parse error")
    })( """(\d+)\.(\d+)(?:\.(\d+))?(?:\-RC(\d+))?""".r, _))
  }
}