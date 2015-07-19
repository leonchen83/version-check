package version

/**
 * Created by leon on 15-7-13.
 */
object VersionCheck {

  def latest(vers: Seq[String]): String = {
    vers.map(parseVersion).max(new Ordering[Version] {
      override def compare(x: Version, y: Version): Int = {
        implicitly[Ordering[(Int, Int, Int, Int)]].compare(x.tuple, y.tuple)
      }
    }).ori
  }

  private[this] def parseVersion(ver: String): Version = {
    val it: Iterator[Char] = ver.iterator
    var next = it.next()
    var num = parseNum(next, it)
    val v1: Int = num._1
    var v2: Int = 0
    var v3: Int = 0
    //compare tricky
    var rc: Int = Int.MaxValue
    next = num._2.get
    if (next == '.') {
      next = it.next()
      num = parseNum(next, it)
      v2 = num._1
      next = num._2.getOrElse(return Version(ver, v1, v2))
    }
    if (next == '.') {
      next = it.next()
      num = parseNum(next, it)
      v3 = num._1
      next = num._2.getOrElse(return Version(ver, v1, v2, v3))
    }
    if (next == '-') {
      next = it.next()
      //ignore case
      if (next == 'R' || next == 'r') {
        next = it.next()
        if (next == 'C' || next == 'c') {
          next = it.next()
          num = parseNum(next, it)
          rc = num._1
          next = num._2.getOrElse(return Version(ver, v1, v2, v3, rc))
        }
      }
    }
    throw new Exception("parse error")
  }

  private[this] def parseNum(ch: Char, it: Iterator[Char]): (Int, Option[Char]) = {
    var next = ch
    val sb = new StringBuilder
    next match {
      case '0' =>
        //avoid leading zeros
        sb.append(next)
        if (it.hasNext) {
          next = it.next()
        } else {
          return (sb.toString().toInt, None)
        }
      case ch if ch > '0' && ch <= '9' =>
        sb.append(next)
        if (it.hasNext) {
          next = it.next()
        } else {
          return (sb.toString().toInt, None)
        }
        while (next >= '0' && next <= '9') {
          sb.append(next)
          if (it.hasNext) {
            next = it.next()
          } else {
            return (sb.toString().toInt, None)
          }
        }
    }
    (sb.toString().toInt, Some(next))
  }
}

case class Version(ori: String, v1: Int, v2: Int, v3: Int = 0, rc: Int = Int.MaxValue) {
  def tuple: (Int, Int, Int, Int) = (v1, v2, v3, rc)
}
