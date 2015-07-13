package version

/**
 * Created by leon on 15-7-13.
 */
import org.scalatest._
import VersionCheck._

class TestVersion extends FlatSpec {
  """"0.13.7", "0.9.10", "0.9.11-RC1"""" should "0.13.7" in {
    assert(latest(List("0.13.7", "0.9.10", "0.9.11-RC1")) == "0.13.7")
  }

  """"0.13.6", "0.13.7-RC1", "0.13.7-RC2""""  should "0.13.7-RC2" in {
    assert(latest(List("0.13.6", "0.13.7-RC1", "0.13.7-RC2")) == "0.13.7-RC2")
  }

  """"0.0.6", "0.0.7", "0.0.7-RC2""""  should "0.0.7-RC1" in {
    assert(latest(List("0.0.6", "0.0.7", "0.0.7-RC2")) == "0.0.7-RC1")
  }
}
