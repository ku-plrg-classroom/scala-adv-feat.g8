package kuplrg

trait Template {
  type True = Unit
  type False = Nothing
  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]
  type <=>[A, B] = (A => B) /\ (B => A)
  type ![A] = A => False

  type Prop1 = [A, B, C] => (
    ((A => B) /\ (B => C)) => (A => C),
  )
  def proof1: Prop1

  type Prop2 = [A, B, C, D] => (
    ((A => B) /\ (B => C) /\ (C => D) /\ (D => A)) => B <=> D,
  )
  def proof2: Prop2

  type Prop3 = [P[_], Q[_, _]] => (
    ([X, Y] => ((P[X] /\ P[Y]) => Q[X, Y])) => ([Z] => ![Q[Z, Z]] => ![P[Z]]),
  )
  def proof3: Prop3

  type Prop4 =
    ([A, B] => (![A] /\ ![B]) => ![A \/ B]) /\
    ([A, B] => ![A \/ B] => (![A] /\ ![B]))
  def proof4: Prop4

  type Prop5 = [A, B, C] => (
    ((![A] /\ B) \/ (![A] /\ C)) => ![A \/ ![B \/ C]],
  )
  def proof5: Prop5

  trait Nums[T] {
    type Num = T
    val zero: Num
    val one: Num
    def add(x: Num, y: Num): Num
    def smul(x: Num, k: Int): Num
    def mul(x: Num, y: Num): Num
  }
  given [T](using nums: Nums[T]): Conversion[Int, T] = _ match
    case 0 => nums.zero
    case 1 => nums.one
    case n => nums.smul(nums.one, n)
  extension [T](t: T)(using nums: Nums[T]) {
    def +(u: T): T = nums.add(t, u)
    def *(k: Int): T = nums.smul(t, k)
    def *(u: T): T = nums.mul(t, u)
  }

  given intNums: Nums[Int] with {
    val zero: Int = 0
    val one: Int = 1
    def add(x: Int, y: Int): Int = x + y
    def smul(x: Int, k: Int): Int = k * x
    def mul(x: Int, y: Int): Int = x * y
  }

  given boolNums: Nums[Boolean] with {
    val zero: Boolean = false
    val one: Boolean = true
    def add(x: Boolean, y: Boolean): Boolean = x || y
    def smul(x: Boolean, k: Int): Boolean = if (k % 2 == 0) false else x
    def mul(x: Boolean, y: Boolean): Boolean = x && y
  }

  case class Tree[T](value: T, children: List[Tree[T]]) {
    override def toString: String =
      s"T($value${children.map(", " + _).mkString})"
  }
  object Tree {
    def apply[T](value: T, children: Tree[T]*): Tree[T] =
      Tree(value, children.toList)
  }
  given treeNums[T: Nums]: Nums[Tree[T]]

  opaque type Matrix[T] = Vector[Vector[T]]
  extension [T](x: Matrix[T]) def apply(i: Int, j: Int): T = x(i)(j)
  object Matrix:
    def apply[T](n: Int)(seq: T*): Matrix[T] = from(n)(seq)
    def from[T](n: Int)(seq: Seq[T]): Matrix[T] =
      if (seq.size != n * n) error("Invalid matrix data")
      else seq.toVector.grouped(n).toVector
  def matrixNums[T: Nums](n: Int): Nums[Matrix[T]]

  inline def unrollMul(inline x: Int, inline n: Int): Int

  def pow(x: Int, n: Int): Int = math.pow(x, n).toInt
  inline def unrollPow(inline x: Int, inline n: Int, inline k: Int): Int
}
