package chap4

object Exe2 extends App {
  def sumOpt(xs: Seq[Option[Double]]): Option[Double] = xs.reduce { (optX, optY) =>
    for {
      x <- optX
      y <- optY
    } yield x + y
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val opts = xs.map { x => if(x.isNaN) None else Some(x) }
    val average = sumOpt(opts).map(_ / opts.size)
    average.flatMap { ave =>
      val vs = opts.map(_.map { x => math.pow(x - ave, 2) })
      sumOpt(vs).map(_ / opts.size)
    }
  }

  assert(variance(Seq(1.0, 2.0, 3.0)).isDefined)
  assert(variance(Seq(1.0, 2.0, Double.NaN)).isEmpty)
}
