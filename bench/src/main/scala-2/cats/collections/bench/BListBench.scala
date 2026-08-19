/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.collections
package bench

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.util.Random

import scala.annotation.tailrec

@State(Scope.Thread)
class BListBench {
  @Param(Array("100", "1000", "10000"))
  var n: Int = _

  var bList: BList[Int] = _
  var list: List[Int] = _
  var vector: Vector[Int] = _

  @Setup
  def setup(): Unit = {
    list = List.fill(n)(Random.nextInt(20) + 1)
    bList = BList.fromList(list)
    vector = list.toVector
  }

  // contrived benchmark #1
  @Benchmark
  def cb1_B_List(bh: Blackhole): Unit = {
    bh.consume(bList.map(x => x * 3).filter(x => x % 4 != 0).foldLeft(0)((a, b) => a + b))
  }
  @Benchmark
  def cb1List(bh: Blackhole): Unit = {
    bh.consume(list.map(x => x * 3).filter(x => x % 4 != 0).foldLeft(0)((a, b) => a + b))
  }
  @Benchmark
  def cb1Vector(bh: Blackhole): Unit = {
    bh.consume(vector.map(x => x * 3).filter(x => x % 4 != 0).foldLeft(0)((a, b) => a + b))
  }

  // contrived benchmark #2
  @Benchmark
  def cb2_B_List(bh: Blackhole): Unit = {
    val builder = BList.newBuilder[Int]
    builder ++= list
    builder ++= list.take(n / 3)
    builder ++= list.drop(n / 3)
    builder ++= list
    for (i <- 0 until (n / 4)) {
      builder += i
    }
    bh.consume(builder.result().map(x => x * 3).map(x => x + 2))
  }
  @Benchmark
  def cb2List(bh: Blackhole): Unit = {
    val builder = List.newBuilder[Int]
    builder ++= list
    builder ++= list.take(n / 3)
    builder ++= list.drop(n / 3)
    builder ++= list
    for (i <- 0 until (n / 4)) {
      builder += i
    }
    bh.consume(builder.result().map(x => x * 3).map(x => x + 2))
  }
  @Benchmark
  def cb2Vector(bh: Blackhole): Unit = {
    val builder = Vector.newBuilder[Int]
    builder ++= list
    builder ++= list.take(n / 3)
    builder ++= list.drop(n / 3)
    builder ++= list
    for (i <- 0 until (n / 4)) {
      builder += i
    }
    bh.consume(builder.result().map(x => x * 3).map(x => x + 2))
  }

  // contrived benchmark #3
  @Benchmark
  def cb3_B_List(bh: Blackhole): Unit = {
    var xs = bList
    for (i <- 1 until 10) {
      xs = xs ++ bList.take(n / i)
    }
    bh.consume(xs.map(x => x * 3).map(x => x + 2))
  }
  @Benchmark
  def cb3List(bh: Blackhole): Unit = {
    var xs = list
    for (i <- 1 until 10) {
      xs = xs ++ list.take(n / i)
    }
    bh.consume(xs.map(x => x * 3).map(x => x + 2))
  }
  @Benchmark
  def cb3Vector(bh: Blackhole): Unit = {
    var xs = vector
    for (i <- 1 until 10) {
      xs = xs ++ vector.take(n / i)
    }
    bh.consume(xs.map(x => x * 3).map(x => x + 2))
  }

  // contrived benchmark #4
  @Benchmark
  def cb4_B_List(bh: Blackhole): Unit = {
    val xs = bList ++ bList
    bh.consume(xs.reverse.map(x => x * 3).map(x => x + 2).reverse)
  }
  @Benchmark
  def cb4List(bh: Blackhole): Unit = {
    val xs = list ++ list
    bh.consume(xs.reverse.map(x => x * 3).map(x => x + 2).reverse)
  }
  @Benchmark
  def cb4Vector(bh: Blackhole): Unit = {
    val xs = vector ++ vector
    bh.consume(xs.reverse.map(x => x * 3).map(x => x + 2).reverse)
  }

  // contrived benchmark #5
  @Benchmark
  def cb5_B_List(bh: Blackhole): Unit = {
    var x = 0
    for (_ <- 0 until 100) {
      x = Random.nextInt(n)
      bh.consume(bList.get(x.toLong))
    }
  }
  @Benchmark
  def cb5List(bh: Blackhole): Unit = {
    var x = 0
    for (_ <- 0 until 100) {
      x = Random.nextInt(n)
      bh.consume(list(x))
    }
  }
  @Benchmark
  def cb5Vector(bh: Blackhole): Unit = {
    var x = 0
    for (_ <- 0 until 100) {
      x = Random.nextInt(n)
      bh.consume(vector(x))
    }
  }

  // contrived benchmark #6
  @Benchmark
  def cb6_B_List(bh: Blackhole): Unit = {
    var x = 0
    for (_ <- 0 until 30) {
      x = Random.nextInt(n)
      bh.consume(bList.take(n))
      bh.consume(bList.drop(n))
    }
    for (_ <- 0 until 10) {
      bh.consume(bList.takeWhile(_ < 19))
      bh.consume(bList.dropWhile(_ > 0))
    }
  }
  @Benchmark
  def cb6List(bh: Blackhole): Unit = {
    var x = 0
    for (_ <- 0 until 30) {
      x = Random.nextInt(n)
      bh.consume(list.take(n))
      bh.consume(list.drop(n))
    }
    for (_ <- 0 until 10) {
      bh.consume(list.takeWhile(_ < 19))
      bh.consume(list.dropWhile(_ > 0))
    }
  }
  @Benchmark
  def cb6Vector(bh: Blackhole): Unit = {
    var x = 0
    for (_ <- 0 until 30) {
      x = Random.nextInt(n)
      bh.consume(vector.take(n))
      bh.consume(vector.drop(n))
    }
    for (_ <- 0 until 10) {
      bh.consume(vector.takeWhile(_ < 19))
      bh.consume(vector.dropWhile(_ > 0))
    }
  }

  // the makeChunk method from fs2 channel (example of real possible usage of blist)
  @Benchmark
  def makeChunkChannel_B_List(bh: Blackhole): Unit = {
    val arr = new Array[Any](n)
    var i = n - 1
    var values = bList
    while (i >= 0) {
      arr(i) = values.headUnsafe
      values = values.tailUnsafe
      i -= 1
    }
    bh.consume(arr)
  }
  @Benchmark
  def makeChunkChannel_List(bh: Blackhole): Unit = {
    val arr = new Array[Any](n)
    var i = n - 1
    var values = list
    while (i >= 0) {
      arr(i) = values.head
      values = values.tail
      i -= 1
    }
    bh.consume(arr)
  }
  @Benchmark
  def makeChunkChannel_Vector(bh: Blackhole): Unit = {
    val arr = new Array[Any](n)
    var i = n - 1
    var values = vector
    while (i >= 0) {
      arr(i) = values.head
      values = values.tail
      i -= 1
    }
    bh.consume(arr)
  }

  // prepending benchmarks
  @Benchmark
  def blistprepend(bh: Blackhole): Unit = {
    var b: BList[Int] = BList.empty
    for (_ <- 0 until n) {
      b = b.prepend(Random.nextInt())
    }
    bh.consume(b)
  }
  @Benchmark
  def listprepend(bh: Blackhole): Unit = {
    var b: List[Int] = List.empty
    for (_ <- 0 until n) {
      b = Random.nextInt() :: b
    }
    bh.consume(b)
  }
  @Benchmark
  def vectorprepend(bh: Blackhole): Unit = {
    var b: Vector[Int] = Vector.empty
    for (_ <- 0 until n) {
      b = Random.nextInt() +: b
    }
    bh.consume(b)
  }

  @Benchmark
  def sumList(bh: Blackhole): Unit = {
    bh.consume(list.foldLeft(0)((acc, a) => acc + a))
  }

  @Benchmark
  def sumBList(bh: Blackhole): Unit = {
    bh.consume(bList.foldLeft(0)((acc, a) => acc + a))
  }

  @Benchmark
  def sumVector(bh: Blackhole): Unit = {
    bh.consume(vector.foldLeft(0)((acc, a) => acc + a))
  }

  // random access compairison
  @Benchmark
  def randomAccessList(bh: Blackhole): Unit = {
    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = list((rand.nextInt() & Int.MaxValue) % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }

    bh.consume(loop(100, 0))
  }
  @Benchmark
  def randomAccessBList(bh: Blackhole): Unit = {
    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = bList.getUnsafe((rand.nextInt() & Int.MaxValue).toLong % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }

    bh.consume(loop(100, 0))
  }
  @Benchmark
  def randomAccessVector(bh: Blackhole): Unit = {
    val rand = new java.util.Random(42)
    @tailrec
    def loop(cnt: Int, acc: Int): Int = {
      val v = vector((rand.nextInt() & Int.MaxValue) % n) + acc
      if (cnt <= 0) v
      else loop(cnt - 1, v)
    }

    bh.consume(loop(100, 0))
  }

  // builder comparison
  @Benchmark
  def ListBuilder(bh: Blackhole): Unit = {
    val builder = List.newBuilder[Int]
    val list2 = list ++ list
    list2.filter(x => x % 2 != 0).map(x => x + 1).map(x => x + 1).foreach { case x =>
      builder += x
    }
    bh.consume(builder.result())
  }
  @Benchmark
  def BListBuilder(bh: Blackhole): Unit = {
    val builder = BList.newBuilder[Int]
    val blist2 = bList ++ bList
    blist2.filter(x => x % 2 != 0).map(x => x + 1).map(x => x + 1).foreach { case x =>
      builder += x
    }
    bh.consume(builder.result())
  }

}
