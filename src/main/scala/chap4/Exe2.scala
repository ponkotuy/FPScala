package chap4

object Exe2 extends App {
  def variance(xs: Seq[Double]): Option[Double] = {
    val opts: Seq[Option[Double]] = xs.map { x => if(x.isNaN) None else Some(x) }
    val average = opts.reduce { (optX, optY) =>
      for {
        x <- optX
        y <- optY
      } yield x + y
    }.map(_ / opts.size)
    average.flatMap { ave =>
      opts.map(_.map { x => math.pow(x - ave, 2) }).reduce { (optX, optY) =>
        for {
          x <- optX
          y <- optY
        } yield x + y
      }.map(_ / opts.size)
    }
  }

  assert(variance(Seq(1.0, 2.0, 3.0)).isDefined)
  assert(variance(Seq(1.0, 2.0, Double.NaN)).isEmpty)
}
