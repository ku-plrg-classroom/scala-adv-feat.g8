package kuplrg

object Implementation extends Template {

  def proof1: Prop1 = ???

  def proof2: Prop2 = ???

  def proof3: Prop3 = ???

  def proof4: Prop4 = ???

  def proof5: Prop5 = ???

  given treeNums[T: Nums]: Nums[Tree[T]] = ???

  def matrixNums[T: Nums](n: Int): Nums[Matrix[T]] = ???

  transparent inline def unrollMul(
    inline x: Int,
    inline n: Int,
  ): Int = ???

  transparent inline def unrollPow(
    inline x: Int,
    inline n: Int,
    inline k: Int,
  ): Int = ???
}
