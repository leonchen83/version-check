package version

/**
 * Created by leon on 15-7-13.
 */
object VersionCheck {

  def latest(vers: Seq[String]): String = {
    vers.map(parseVersion).max(new Ordering[Version] {
      override def compare(x: Version, y: Version): Int = {
        val c1 = x.v1.compareTo(y.v1)
        if (c1 == 0) {
          val c2 = x.v2.compareTo(y.v2)
          if (c2 == 0) {
            val c3 = x.v3.compareTo(y.v3)
            if (c3 == 0) {
              if (x.rc.isEmpty && y.rc.isEmpty) {
                1
              } else if (x.rc.isEmpty) {
                -1
              } else if (y.rc.isEmpty) {
                1
              } else {
                x.rc.get.compareTo(y.rc.get)
              }
            } else {
              c3
            }
          } else {
            c2
          }
        } else {
          c1
        }
      }
    }).ori
  }

  def parseVersion(ver: String): Version = {
    val rs = new Version()
    rs.ori = ver
    val it: Iterator[Char] = ver.iterator
    var next = it.next()
    var num = parseNum(next, it)
    rs.v1 = num._1
    next = num._2.get
    if (next == '.') {
      next = it.next()
      num = parseNum(next, it)
      rs.v2 = num._1
      next = num._2.getOrElse(return rs)
    }
    if (next == '.') {
      next = it.next()
      num = parseNum(next, it)
      rs.v3 = num._1
      next = num._2.getOrElse(return rs)
    }
    if (next == '-') {
      next = it.next()
      if (next == 'R') {
        next = it.next()
        if (next == 'C') {
          next = it.next()
          num = parseNum(next, it)
          rs.rc = Some(num._1)
          next = num._2.getOrElse(return rs)
        }
      }
    }
    throw new Exception("parse error")
  }

  def parseNum(ch: Char, it: Iterator[Char]): (Int, Option[Char]) = {
    var next = ch
    val sb = new StringBuilder
    next match {
      case '0' =>
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

class Version {
  var v1: Int = _
  var v2: Int = _
  var v3: Int = 0
  var ori: String = _
  var rc: Option[Int] = None

  override def toString = s"Version(v1=$v1, v2=$v2, v3=$v3, ori=$ori, rc=$rc)"
}
