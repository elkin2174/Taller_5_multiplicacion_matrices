import scala.util.Random
import common.*
package object Matrices {
  val random = new Random()

  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long:Int, vals:Int): Matriz = {
    val v = Vector.fill(long,long){random.nextInt(vals)}
    v
  }

  def vectorAlAzar(long: Int, vals:Int): Vector[Int] = {
    val v = Vector.fill(long){random.nextInt(vals)}
    v
  }

  def prodPunto(v1:Vector[Int], v2:Vector[Int]):Int ={
    (v1 zip v2).map({ case (i, j) => (i*j)}).sum
  }

  def transpuesta(m:Matriz):Matriz = {
    val l = m.length
    Vector.tabulate(l,l)((i,j) => m(j)(i))
  }

  def multMatriz(m1:Matriz, m2:Matriz): Matriz = {
    val m2T = transpuesta(m2)
    val l = m1.length
    Vector.tabulate(l,l){ (i,j) => prodPunto(m1(i),m2T(j)) }
  }

  def multMatrizPar(m1:Matriz, m2:Matriz): Matriz={
    val m2T = transpuesta(m2)
    val len = m1.length
    val l: Int = len / 2

    val (ml, mlrest) = parallel(
      Vector.tabulate(l, len)((i, j) => prodPunto(m1(i), m2T(j))),
      Vector.tabulate(l + len%2, len)((i, j) => prodPunto(m1(l+i), m2T(j)))
    )
    ml++mlrest
  }

  def subMatriz(m:Matriz, i:Int, j:Int, l:Int): Matriz = {
    Vector.tabulate(l,l){ (x,y) => m(i+x)(j+y) }
  }
  
  def sumMatriz(m1:Matriz, m2:Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l,l)((i,j) => m1(i)(j) + m2(i)(j))
  }

}




