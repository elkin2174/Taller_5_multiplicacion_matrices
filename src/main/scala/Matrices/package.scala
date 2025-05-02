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

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
      val n = m1.length
      // Caso base: matriz 1x1
      if (n == 1) {
        Vector(Vector(m1(0)(0) * m2(0)(0)))
      } else {
        val m = n / 2
        // Particionar en submatrices de tamaño n/2 x n/2
        val A11 = subMatriz(m1, 0, 0, m);    val A12 = subMatriz(m1, 0, m, m)
        val A21 = subMatriz(m1, m, 0, m);    val A22 = subMatriz(m1, m, m, m)
        val B11 = subMatriz(m2, 0, 0, m);    val B12 = subMatriz(m2, 0, m, m)
        val B21 = subMatriz(m2, m, 0, m);    val B22 = subMatriz(m2, m, m, m)
        // Calcular bloques de C recursivamente y sumar
        val C11 = sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21))
        val C12 = sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22))
        val C21 = sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21))
        val C22 = sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))
        // Combinar bloques en la matriz resultante de tamaño n x n
        Vector.tabulate(n, n){ (i, j) =>
          if (i < m && j < m) C11(i)(j)
          else if (i < m && j >= m) C12(i)(j - m)
          else if (i >= m && j < m) C21(i - m)(j)
          else C22(i - m)(j - m)
        }
      }
  }
  
}




