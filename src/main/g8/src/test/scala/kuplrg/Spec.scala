package kuplrg

import Implementation.{Tree => T, Matrix => M, *, given}

class Spec extends SpecBase {

  val inc: Int => Int = _ + 1
  val double: Int => Int = _ * 2
  val square: Int => Int = x => x * x
  val length: String => Int = _.length
  val not: Boolean => Boolean = !_
  val intToStr: Int => String = _.toString
  val intToBool: Int => Boolean = _ != 0
  val boolToInt: Boolean => Int = if _ then 1 else 0
  val toLower: String => String = _.toLowerCase
  val True = ()
  def not[T](x: T): False = error(s"not $x")
  val t = true
  val f = false
  type I = Int
  type S = String
  type B = Boolean
  type L[X] = List[X]
  type N[X, Y] = List[X | Y]
  type O[X] = Option[X]
  type P[X] = X
  type Q[X, Y] = (X, Y)
  val t1 = T(1, T(2, T(3)), T(3))
  val t2 = T(3, T(5, T(7), T(11)), T(13, T(19)))
  val t3 = T(4, T(7, T(9, T(12))), T(3, T(5)))

  // ---------------------------------------------------------------------------
  // tests for `proof1`
  // ---------------------------------------------------------------------------
  {
    lazy val x11 = proof1(inc, double)(5)
    test(x11, 12, weight = 4)
    lazy val x12 = proof1(square, inc)(12)
    test(x12, 145, weight = 4)
    lazy val x13 = proof1(not, not)(true)
    test(x13, true, weight = 2)
  }

  // ---------------------------------------------------------------------------
  // tests for `proof2`
  // ---------------------------------------------------------------------------
  {
    lazy val (f, g) = proof2(((inc, intToBool), not), boolToInt)
    lazy val x21 = (f(5), g(true))
    test(x21, (false, 2), weight = 4)
    lazy val (x, y) = proof2(((length, intToBool), boolToInt), intToStr)
    lazy val x22 = (x(42), y(7))
    test(x22, (1, 1), weight = 4)
    lazy val (b, c) = proof2(((not, not), not), not)
    lazy val x23 = (b(true), c(false))
    test(x23, (true, false), weight = 2)
  }

  // ---------------------------------------------------------------------------
  // tests for `proof3`
  // ---------------------------------------------------------------------------
  {
    lazy val x31 = proof3[P, Q](
      [X, Y] => (p: (X, Y)) => p,
    )(not)(true)
    testExc(x31, "not (true,true)", weight = 4)
    lazy val x32 = proof3[O, N](
      [X, Y] => (p: (O[X], O[Y])) => p._1.toList ++ p._2,
    )(not)(Some(42))
    testExc(x32, "not List(42, 42)", weight = 4)
    lazy val x33 = proof3[L, N](
      [X, Y] => (p: (L[X], L[Y])) => p._1 ++ p._2,
    )(not)(List("a", "b", "c"))
    testExc(x33, "not List(a, b, c, a, b, c)", weight = 2)
  }

  // ---------------------------------------------------------------------------
  // tests for `proof4`
  // ---------------------------------------------------------------------------
  {
    lazy val (f, g) = proof4
    lazy val x41 = f(not[I], not[S])(Left(42))
    testExc(x41, "not 42", weight = 2)
    lazy val x42 = g(not[B \/ I])._1(true)
    testExc(x42, "not Left(true)", weight = 3)
    lazy val x43 = f(not[S], not[B])(Right(false))
    testExc(x43, "not false", weight = 2)
    lazy val x44 = g(not[I \/ S])._2("abc")
    testExc(x44, "not Right(abc)", weight = 3)
  }

  // ---------------------------------------------------------------------------
  // tests for `proof5`
  // ---------------------------------------------------------------------------
  {
    lazy val x51 = proof5(Left(not[I], true))(Left(42))
    testExc(x51, "not 42", weight = 2)
    lazy val x52 = proof5(Left(not[I], "abc"))(Right(not[S \/ B]))
    testExc(x52, "not Left(abc)", weight = 3)
    lazy val x53 = proof5(Right(not[B], 42))(Left(false))
    testExc(x53, "not false", weight = 2)
    lazy val x54 = proof5(Right(not[S], true))(Right(not[I \/ B]))
    testExc(x54, "not Right(true)", weight = 3)
  }

  // ---------------------------------------------------------------------------
  // tests for `treeNums`
  // ---------------------------------------------------------------------------
  {
    val bt1 = T(t, T(f, T(t), T(t)), T(f))
    val bt2 = T(t, T(f, T(t), T(f)), T(f, T(t)))
    val bt3 = T(t, T(t, T(t), T(f)), T(f, T(f)))
    val tt1 = T(t1, T(t2, T(t1)), T(t3))
    val tt2 = T(t3, T(t1, T(t2)), T(t1, T(t3)))
    val tt3 = T(t2, T(t3, T(t1)), T(t2, T(t2)))
    test(0: T[Int], T(0))
    test(0: T[Boolean], T(f))
    test(0: T[T[Int]], T(T(0)))
    test(1: T[Int], T(1))
    test(1: T[Boolean], T(t))
    test(1: T[T[Int]], T(T(1)))
    test(t1 + t2 + t3, T(8, T(14, T(19)), T(19)))
    test(bt1 + bt2 + bt3, T(t, T(t, T(t), T(t)), T(f)))
    test(
      tt1 + tt2 + tt3,
      T(
        T(8, T(14, T(19)), T(19)),
        T(T(8, T(14, T(19)), T(19)), T(T(5, T(9, T(13)), T(19)))),
        T(T(8, T(14, T(19)), T(19))),
      ),
    )
    test(t1 * t2 * t3, T(12, T(70, T(189)), T(117)))
    test(bt1 * bt2 * bt3, T(t, T(f, T(t), T(f)), T(f)))
    test(
      tt1 * tt2 * tt3,
      T(
        T(12, T(70, T(189)), T(117)),
        T(T(12, T(70, T(189)), T(117)), T(T(3, T(20, T(63)), T(117)))),
        T(T(12, T(70, T(189)), T(117))),
      ),
    )
    test(t1 * 42, T(42, T(84, T(126)), T(126)))
    test(bt1 * 7, T(t, T(f, T(t), T(t)), T(f)))
    test(
      tt1 * 42,
      T(
        T(42, T(84, T(126)), T(126)),
        T(
          T(126, T(210, T(294), T(462)), T(546, T(798))),
          T(T(42, T(84, T(126)), T(126))),
        ),
        T(T(168, T(294, T(378, T(504))), T(126, T(210)))),
      ),
    )
  }

  // ---------------------------------------------------------------------------
  // tests for `matrixNums`
  // ---------------------------------------------------------------------------
  {
    given m1: Nums[M[I]] = matrixNums(2)
    given m2: Nums[M[T[I]]] = matrixNums(3)
    given m3: Nums[M[M[I]]] = matrixNums(2)
    def mi(seq: Int*): M[I] = M(2)(seq: _*)
    val mi0 = mi(0, 0, 0, 0)
    val mi1 = mi(1, 0, 0, 1)
    val mi2 = mi(3, 6, 2, 7)
    val mi3 = mi(9, 3, 6, 2)
    def mt(seq: T[I]*): M[T[I]] = M(3)(seq: _*)
    val mt0 = mt(T(0), T(0), T(0), T(0), T(0), T(0), T(0), T(0), T(0))
    val mt1 = mt(T(1), T(0), T(0), T(0), T(1), T(0), T(0), T(0), T(1))
    val mt2 = mt(T(3), T(6), T(2), T(7), T(3), T(6), T(2), T(7), T(3))
    val mt3 = mt(T(9), T(3), T(6), T(2), T(9), T(3), T(6), T(2), T(9))
    def mm(seq: M[I]*): M[M[I]] = M(2)(seq: _*)
    val mm0 = mm(mi0, mi0, mi0, mi0)
    val mm1 = mm(mi1, mi0, mi0, mi1)
    val mm2 = mm(mi2, mi3, mi0, mi1)
    val mm3 = mm(mi1, mi3, mi1, mi2)
    test(0: M[I], mi0)
    test(0: M[T[I]], mt0)
    test(0: M[M[I]], mm0)
    test(1: M[I], mi1)
    test(1: M[T[I]], mt1)
    test(1: M[M[I]], mm1)
    test(mi1 + mi2 + mi3, mi(13, 9, 8, 10))
    test(
      mt1 + mt2 + mt3,
      mt(T(13), T(9), T(8), T(9), T(13), T(9), T(8), T(9), T(13)),
    )
    test(
      mm1 + mm2 + mm3,
      mm(mi(5, 6, 2, 9), mi(18, 6, 12, 4), mi(1, 0, 0, 1), mi(5, 6, 2, 9)),
    )
    test(mi1 * mi2 * mi3, mi(63, 21, 60, 20))
    test(
      mt1 * mt2 * mt3,
      mt(T(51), T(67), T(54), T(105), T(60), T(105), T(50), T(75), T(60)),
    )
    test(
      mm1 * mm2 * mm3,
      mm(mi(12, 9, 8, 9), mi(96, 96, 82, 70), mi(1, 0, 0, 1), mi(3, 6, 2, 7)),
    )
    test(mi3 * 7, mi(63, 21, 42, 14))
    test(
      mt3 * 7,
      mt(T(63), T(21), T(42), T(14), T(63), T(21), T(42), T(14), T(63)),
    )
    test(
      mm3 * 3,
      mm(mi(3, 0, 0, 3), mi(27, 9, 18, 6), mi(3, 0, 0, 3), mi(9, 18, 6, 21)),
    )
  }

  // ---------------------------------------------------------------------------
  // tests for `unrollMul`
  // ---------------------------------------------------------------------------
  {
    val x = 2
    test(codeOf(unrollMul(1, 15)), "15")
    test(codeOf(unrollMul(17, 3)), "51")
    test(codeOf(unrollMul(25, 31)), "775")
    test(codeOf(unrollMul(x, 0)), "0")
    test(codeOf(unrollMul(x, 1)), "x")
    test(codeOf(unrollMul(x, 2)), "x.*(2)")
    test(codeOf(unrollMul(x, 5)), "x.*(2).*(2).+(x)")
    test(codeOf(unrollMul(x, 11)), "x.*(2).*(2).+(x).*(2).+(x)")
    test(codeOf(unrollMul(x, 15)), "x.*(2).+(x).*(2).+(x).*(2).+(x)")
    test(codeOf(unrollMul(x, 42)), "x.*(2).*(2).+(x).*(2).*(2).+(x).*(2)")
  }
  // ---------------------------------------------------------------------------
  // tests for `unrollPow`
  // ---------------------------------------------------------------------------
  {
    val x = 2
    test(codeOf(unrollPow(2, 5, 3)), "8.*(pow(2, 2))")
    test(codeOf(unrollPow(2, 10, 15)), "1024")
    test(codeOf(unrollPow(2, 8, 8)), "256")
    test(codeOf(unrollPow(2, 8, 6)), "64.*(pow(2, 2))")
    test(codeOf(unrollPow(2, 8, 0)), "pow(2, 8)")
    test(codeOf(unrollPow(x, 5, 7)), "x.*(x).*(x).*(x).*(x)")
    test(codeOf(unrollPow(x, 5, 2)), "x.*(x).*(pow(x, 3))")
    test(codeOf(unrollPow(x, 7, 5)), "x.*(x).*(x).*(x).*(x).*(pow(x, 2))")
    test(codeOf(unrollPow(x, 7, 2)), "x.*(x).*(pow(x, 5))")
    test(codeOf(unrollPow(x, 7, 0)), "pow(x, 7)")
  }

  /* Write your own tests */
}
