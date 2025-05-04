import scala.util.Random
import common.*

import scala.collection.parallel.immutable.ParVector
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
    (v1 zip v2).map({ case (i, j) => i*j}).sum
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
      n match
        case 1 => Vector(Vector(m1(0)(0) * m2(0)(0)))
        case _ =>
          val m = n / 2

          val A11 = subMatriz(m1, 0, 0, m);    val A12 = subMatriz(m1, 0, m, m)
          val A21 = subMatriz(m1, m, 0, m);    val A22 = subMatriz(m1, m, m, m)
          val B11 = subMatriz(m2, 0, 0, m);    val B12 = subMatriz(m2, 0, m, m)
          val B21 = subMatriz(m2, m, 0, m);    val B22 = subMatriz(m2, m, m, m)


          val C11 = sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21))
          val C12 = sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22))
          val C21 = sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21))
          val C22 = sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))

          Vector.tabulate(n, n){ (i, j) =>
            (i < m, j < m) match
              case (true, true) => C11(i)(j)
              case (true, false) => C12(i)(j - m)
              case (false, true) => C21(i - m)(j)
              case (false, false) => C22(i - m)(j - m)
          }
  }

  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    n match
      //Aquí se puede definir el umbral de paralelizacion en una potencia de 2 y minimo 2
      case 2 =>
        //Una vez alcanza el umbral se utiliza la version sin paralelizar
        multMatrizRec(m1, m2)
      case _ =>
        val m = n / 2
        val (a11, a12, a21, a22) = parallel(
          subMatriz(m1, 0, 0, m), subMatriz(m1, 0, m, m),
          subMatriz(m1, m, 0, m), subMatriz(m1, m, m, m)
        )
        val (b11, b12, b21, b22) = parallel(
          subMatriz(m2, 0, 0, m), subMatriz(m2, 0, m, m),
          subMatriz(m2, m, 0, m), subMatriz(m2, m, m, m)
        )
        val (a11b11, b12b21, a11b12, a12b22) = parallel(
          multMatrizRec(a11, b11), multMatrizRec(a12, b21),
          multMatrizRec(a11, b12), multMatrizRec(a12, b22)
        )
        val (a21b11, a22b21, a21b12, a22b22) = parallel(
          multMatrizRec(a21, b11), multMatrizRec(a22, b21),
          multMatrizRec(a21, b12), multMatrizRec(a22, b22)
        )
        val (c11,c12,c21,c22) = parallel(
          sumMatriz(a11b11,b12b21),
          sumMatriz(a11b12, a12b22),
          sumMatriz(a21b11, a22b21),
          sumMatriz(a21b12, a22b22)
        )
        val (m1112,m2122) = parallel(
          Vector.tabulate(m, n) { (i, j) =>
            if (j < m) c11(i)(j)
            else c12(i)(j - m)
          },
          Vector.tabulate(m, n) { (i, j) =>
            if (j < m) c21(i)(j)
            else c22(i)(j - m)
          }
        )
        m1112++m2122
  }

  def restaMatriz(m1:Matriz,m2:Matriz): Matriz = {
    val l = m1.length
    Vector.tabulate(l,l)((i,j) => m1(i)(j) - m2(i)(j))
  }

  def multStrassen(m1:Matriz,m2:Matriz): Matriz = {
    val n = m1.length
    val m = n/2
    n match
      case 1 => Vector(Vector(m1(0)(0) * m2(0)(0)))
      case _ =>
        val A11 = subMatriz(m1, 0, 0, m); val A12 = subMatriz(m1, 0, m, m)
        val A21 = subMatriz(m1, m, 0, m); val A22 = subMatriz(m1, m, m, m)
        val B11 = subMatriz(m2, 0, 0, m); val B12 = subMatriz(m2, 0, m, m)
        val B21 = subMatriz(m2, m, 0, m); val B22 = subMatriz(m2, m, m, m)

        val S1 = restaMatriz(B12,B22); val S2 = sumMatriz(A11,A12)
        val S3 = sumMatriz(A21,A22); val S4 = restaMatriz(B21,B11)
        val S5 = sumMatriz(A11,A22); val S6 = sumMatriz(B11,B22)
        val S7 = restaMatriz(A12,A22); val S8 = sumMatriz(B21,B22)
        val S9 = restaMatriz(A11, A21); val S10 = sumMatriz(B11,B12)

        val P1 = multStrassen(A11,S1); val P2 = multStrassen(S2,B22)
        val P3 = multStrassen(S3,B11); val P4 = multStrassen(A22,S4)
        val P5 = multStrassen(S5,S6); val P6 = multStrassen(S7,S8)
        val P7 = multStrassen(S9,S10)

        val C11 = sumMatriz(restaMatriz(sumMatriz(P5,P4),P2),P6)
        val C12 = sumMatriz(P1,P2)
        val C21 = sumMatriz(P3,P4)
        val C22 = restaMatriz(restaMatriz(sumMatriz(P5,P1),P3),P7)


        Vector.tabulate(n, n){ (i, j) =>
          (i < m, j < m) match
            case (true, true) => C11(i)(j)
            case (true, false) => C12(i)(j - m)
            case (false, true) => C21(i - m)(j)
            case (false, false) => C22(i - m)(j - m)
        }

  }

  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m = n / 2
    n match
      case 1 => Vector(Vector(m1(0)(0) * m2(0)(0)))
      case _ =>
        val (a11, a12, a21, a22) = parallel(
          subMatriz(m1, 0, 0, m), subMatriz(m1, 0, m, m),
          subMatriz(m1, m, 0, m), subMatriz(m1, m, m, m)
        )
        val (b11, b12, b21, b22) = parallel(
          subMatriz(m2, 0, 0, m), subMatriz(m2, 0, m, m),
          subMatriz(m2, m, 0, m), subMatriz(m2, m, m, m)
        )
        val (s1, s2, s3, s4) = parallel(
          restaMatriz(b12, b22),
          sumMatriz(a11, a12),
          sumMatriz(a21, a22),
          restaMatriz(b21, b11)
        )
        val (s5, s6, s7, s8) = parallel(
          sumMatriz(a11, a22),
          sumMatriz(b11, b22),
          restaMatriz(a12, a22),
          sumMatriz(b21, b22)
        )
        val (s9,s10) = parallel(
          restaMatriz(a11, a21),
          sumMatriz(b11, b12)
        )
        val (p1, p2, p3, p4) = parallel(
          multStrassenPar(a11, s1), 
          multStrassenPar(s2, b22), 
          multStrassenPar(s3, b11),
          multStrassenPar(a22, s4)
        )
        val (p5,(p6, p7)) = parallel(
          multStrassenPar(s5, s6),
          parallel(
            multStrassenPar(s7, s8),
            multStrassenPar(s9, s10)
          )
        )
        val (c11, c12, c21, c22) = parallel(
          sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6),
          sumMatriz(p1, p2),
          sumMatriz(p3, p4),
          restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)
        )
        val (m1112A,m2122B) = parallel(
          Vector.tabulate(m, n) { (i, j) =>
            if (j < m) c11(i)(j)
            else c12(i)(j - m)
          },
          Vector.tabulate(m, n) { (i, j) =>
            if (j < m) c21(i)(j)
            else c22(i)(j - m)
          }
        )
        m1112A++m2122B
  }

  def prodPuntoParD(v1:ParVector[Int],v2:ParVector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => i*j}).sum
  }

}




