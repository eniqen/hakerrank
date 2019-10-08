import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import com.github.eniqen.Error.{InvalidLength, InvalidRangeValue, LiError, TiError}
import com.github.eniqen.OrderProcessor
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{Assertion, FunSuite, MustMatchers}

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
class OrderProcessorTest extends FunSuite with MustMatchers with TypeCheckedTripleEquals {

  test("provided test data case 1") {
    val input =
      """3
        |0 3
        |1 9
        |2 6
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Right(9L)))
  }

  test("provided test data case 2") {
    val input =
      """3
        |0 9
        |1 3
        |2 5
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Right(11L)))
  }

  test("provided test data case 3") {
    val input =
      """2
        |0 9
        |10 4
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Right(6L)))
  }

  test("provided test data when empty") {
    val input = ""
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Right(0L)))
  }

  test("provided test data if only clientsCount we have") {
    val input = "2"
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Right(0L)))
  }

  test("provided error when range less then 0") {
    val input =
      """-2
        |0 9
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(InvalidRangeValue))))
  }

  test("provided error when range bigger then 100000") {
    val input =
      """1000000
        |0 9
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(InvalidRangeValue))))
  }

  test("provided error when ti range less then 0") {
    val input =
      """1
        |-1 9
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(TiError(1)))))
  }

  test("provided error when ti range bigger then 1000000000") {
    val input =
      """1
        |1000000001 9
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(TiError(1)))))
  }

  test("provided error when li range less then 1") {
    val input =
      """1
        |1 0
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(LiError(1)))))
  }

  test("provided error when li range bigger then 1000000000") {
    val input =
      """1
        |1 1000000001
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(LiError(1)))))
  }

  test("provided line size error") {
    val input =
      """2
        | 0
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(InvalidLength))))
  }

  test("provided collected errors") {
    val input =
      """2
        |-1 1
        | 2 -1
        |""".stripMargin
    fixture(input)(inStream => OrderProcessor.process(inStream) must ===(Left(List(TiError(1), LiError(2)))))
  }

  private def fixture(in: String)(assert: InputStream => Assertion): Unit = {
    val inStream: InputStream = new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8))
    try assert(inStream) finally inStream.close()
  }
}
