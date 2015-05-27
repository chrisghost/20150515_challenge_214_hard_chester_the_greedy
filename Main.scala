object Main extends App {

  def getPts(fileName: String) = io.Source.fromFile(fileName).getLines.toList.drop(1).map { line =>
    val v = line.split(' ').map(_.toDouble)
    Point(v(0), v(1))
  }

  val start = Point(0.5, 0.5)

  @scala.annotation.tailrec
  def rec(start: Point, pts: Seq[Point], d: List[Double]): List[Double] = pts match {
    case e :: Nil => d :+ start.dst(e)
    case e :: tail => {
      val (p, dst, i) = start.closest(pts)
      rec(
        p
      , pts.take(i) ++ pts.drop(i+1)
      , d :+ dst
      )
    }
  }

  def time[A](f: => A): A = {
    val t1 = System.currentTimeMillis
    val r = f
    println((System.currentTimeMillis-t1)/1000.0f + " seconds")
    r
  }

  println("6 points", time {
    rec(start, getPts("input6"), Nil).reduce(_+_)
  })
  println("100 points", time{rec(start, getPts("input100"), Nil).reduce(_+_)})
  println("1k points", time{rec(start, getPts("input1000"), Nil).reduce(_+_)})
  println("10k points", time{rec(start, getPts("input10000"), Nil).reduce(_+_)})
  println("100k points", time{rec(start, getPts("input100000"), Nil).reduce(_+_)})
}

case class Point(x: Double, y: Double) {
  def dst(b: Point) = scala.math.sqrt(scala.math.pow(x-b.x, 2) + scala.math.pow(y-b.y, 2))
  def closest(pts: Seq[Point]) = {
    var minDst = Integer.MAX_VALUE
    pts.zipWithIndex.map { case (p, i) => {
      Some((p, dst(p), i))
    } }.filter(_.isDefined).minBy(_._2)
  }
}
