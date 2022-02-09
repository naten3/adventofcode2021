package adventofcode2021

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class ReadResult(versionTotal: Int, bitsRead: Int, value: Long)

class BitIterator {
  val hexToByte = (0 to 15)
    .map(i => {
      val hex = i.toHexString.toUpperCase.charAt(0)
      val bin = i.toByte
      hex -> bin
    })
    .toMap

  var iterator = Source.fromResource("day16.txt").iter
  var hexPosition = 0
  var currentHex: Option[Byte] = None

  getNextHex()

  def getNextHex() = {
    if (iterator.hasNext) {
      currentHex = Some(hexToByte(iterator.next()))
      hexPosition = 0
    }
  }

  def getNextBit(): Char = {
    val result = (currentHex.get >> (3 - hexPosition) & 1)
    (result + '0').toChar
  }

  def next(): Char = {
    val returnVal = getNextBit()
    hexPosition = hexPosition + 1
    if (hexPosition == 4) {
      currentHex = None
      getNextHex()
    }
    returnVal
  }

  def next(n: Int): Int = {
    val str = (1 to n).map(_ => next()).mkString
    Integer.parseInt(str, 2)
  }

  def readLiteral(): ReadResult = {
    var total: Long = 0L
    def readBit(bit: Char): Unit = {
      total = total * 2 + { if (bit == '1') 1 else 0 }
    }

    var isLast = false
    var bitsRead = 0
    do {
      isLast = next(1) == 0
      (0 to 3).foreach(_ => readBit(next()))
      bitsRead = bitsRead + 5
    } while (!isLast)
    ReadResult(0, bitsRead, total)
  }

  def hasNext() = currentHex.isDefined
}

class OperatorHandler(operator: Int) {
  val functionMap = Map[Int, (Long, Long) => Long](
    0 ->
      (_ + _),
    1 -> (_ * _),
    2 -> (Math.min(_, _)),
    3 -> (Math.max(_, _))
  )

  val compMap = Map[Int, (Long, Long) => Long](
    5 ->
      ((a, b) => if (a > b) 1 else 0),
    6 ->
      ((a, b) => if (a < b) 1 else 0),
    7 ->
      ((a, b) => if (a == b) 1 else 0)
  )

  val accMap = Map[Int, Long](
    0 -> 0,
    1 -> 1,
    2 -> Integer.MAX_VALUE,
    3 -> Integer.MIN_VALUE
  )

  val values = ListBuffer[Long]()

  def addValue(i: Long) = values.append(i)

  def evaluateComp() = {
    if (values.length > 2 || values.length < 0) {
      throw new RuntimeException("list must have two values")
    }

    compMap(operator)(values(0), values(1))
  }

  def evaluateOperation() =
    values.foldLeft(accMap(operator))(functionMap(operator))

  def evaluate() = {
    if (operator > 4)
      evaluateComp()
    else
      evaluateOperation()
  }
}

object Day16 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def getVersionAndType(iterator: BitIterator) = {
    val version = iterator.next(3)
    val t = iterator.next(3)
    (version, t)
  }

  def readOperatorPacket(it: BitIterator, operator: Int): ReadResult = {
    val opHandler = new OperatorHandler(operator)
    var bitsRead = 0
    var versionTotal = 0
    val lengthType = it.next(1)
    bitsRead = bitsRead + 1

    var readValue: Long = 0
    if (lengthType == 0) {
      val remainingBits = it.next(15)
      bitsRead = bitsRead + 15
      val readResult = readSubPackets(it, remainingBits, opHandler)
      bitsRead = bitsRead + readResult.bitsRead
      versionTotal = versionTotal + readResult.versionTotal
      readValue = readResult.value
    } else {
      val numPackets = it.next(11)
      bitsRead = bitsRead + 11
      val readResult = readNSubPackets(it, numPackets, opHandler)
      bitsRead = bitsRead + readResult.bitsRead
      versionTotal = versionTotal + readResult.versionTotal
      readValue = readResult.value
    }
    ReadResult(versionTotal, bitsRead, readValue)
  }

  def readSubPackets(it: BitIterator,
                     remainingBits: Int,
                     operatorHandler: OperatorHandler): ReadResult = {
    var bitsRead = 0
    var versionTotal = 0
    while (bitsRead < remainingBits) {
      val rr = readNext(it)
      bitsRead = bitsRead + rr.bitsRead
      versionTotal = versionTotal + rr.versionTotal
      operatorHandler.addValue(rr.value)
    }
    ReadResult(versionTotal, bitsRead, operatorHandler.evaluate())
  }

  def readNSubPackets(it: BitIterator,
                      numSubpackets: Int,
                      operatorHandler: OperatorHandler): ReadResult = {
    var bitsRead = 0
    var versionTotal = 0
    (1 to numSubpackets).foreach(_ => {
      val rr = readNext(it)
      bitsRead = bitsRead + rr.bitsRead
      versionTotal = versionTotal + rr.versionTotal
      operatorHandler.addValue(rr.value)
    })
    ReadResult(versionTotal, bitsRead, operatorHandler.evaluate())
  }

  def readNext(it: BitIterator): ReadResult = {
    var bitsRead = 0
    var versionTotal = 0
    val (version, t) = getVersionAndType(it)
    versionTotal = versionTotal + version
    bitsRead = bitsRead + 6
    if (t != 4) {
      val operatorResult = readOperatorPacket(it, t)
      versionTotal = versionTotal + operatorResult.versionTotal
      bitsRead = bitsRead + operatorResult.bitsRead
      ReadResult(versionTotal, bitsRead, operatorResult.value)
    } else {
      val readLiteralResult = readLiteralPacket(it)
      bitsRead = bitsRead + readLiteralResult.bitsRead
      ReadResult(versionTotal, bitsRead, readLiteralResult.value)
    }
  }

  def readLiteralPacket(it: BitIterator): ReadResult = {
    it.readLiteral()
  }

  def part1(): Int = {
    val iterator = new BitIterator()
    readNext(iterator).versionTotal
  }

  def part2(): Long = {
    val iterator = new BitIterator()
    readNext(iterator).value
  }

}
