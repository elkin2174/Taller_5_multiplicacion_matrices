import Matrices._
import Benchmark._
//Ejecucion reducida para una ejecucion mas liviana
for{
  i <- 1 to 8
  m1 = matrizAlAzar(math.pow(2,i).toInt,2)
  m2 = matrizAlAzar(math.pow(2,i).toInt,2)
} yield (compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2),math.pow(2,i).toInt)
for{
  i <- 1 to 8
  m1 = matrizAlAzar(math.pow(2,i).toInt,2)
  m2 = matrizAlAzar(math.pow(2,i).toInt,2)
} yield (compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2),math.pow(2,i).toInt)
for{
  i <- 1 to 8
  m1 = matrizAlAzar(math.pow(2,i).toInt,2)
  m2 = matrizAlAzar(math.pow(2,i).toInt,2)
} yield (compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2),math.pow(2,i).toInt)
/*
//Ejecucion completa, puede tardar demasiado. Descomentar para prueba completa.
for{
  i <- 1 to 10
  m1 = matrizAlAzar(math.pow(2,i).toInt,2)
  m2 = matrizAlAzar(math.pow(2,i).toInt,2)
} yield (compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2),math.pow(2,i).toInt)
for{
  i <- 1 to 10
  m1 = matrizAlAzar(math.pow(2,i).toInt,2)
  m2 = matrizAlAzar(math.pow(2,i).toInt,2)
} yield (compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2),math.pow(2,i).toInt)
for{
  i <- 1 to 10
  m1 = matrizAlAzar(math.pow(2,i).toInt,2)
  m2 = matrizAlAzar(math.pow(2,i).toInt,2)
} yield (compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2),math.pow(2,i).toInt)
*/
// Ejecucion comparativa de parelizacion de datos
for {
  i <- 1 to 1000 by 100
} yield (compararProdPunto(i), i)

//Ejecucion de prueba de las demas funciones.
val m1A = matrizAlAzar(math.pow(2,1).toInt,2)
val m2A = matrizAlAzar(math.pow(2,2).toInt,2)
val m3A = matrizAlAzar(math.pow(2,6).toInt,2)
val m4A = matrizAlAzar(math.pow(2,4).toInt,2)
val m5A = matrizAlAzar(math.pow(2,7).toInt,2)

val m1B = matrizAlAzar(math.pow(2,1).toInt,2)
val m2B = matrizAlAzar(math.pow(2,2).toInt,2)
val m3B = matrizAlAzar(math.pow(2,6).toInt,2)
val m4B = matrizAlAzar(math.pow(2,4).toInt,2)
val m5B = matrizAlAzar(math.pow(2,7).toInt,2)

val V1A = vectorAlAzar(1,2)
val V2A = vectorAlAzar(2,2)
val V3A = vectorAlAzar(3,2)
val V4A = vectorAlAzar(5,2)
val V5A = vectorAlAzar(6,2)

multMatriz(m1A,m1B)
multMatriz(m2A,m2B)
multMatriz(m3A,m3B)
multMatriz(m4A,m4B)
multMatriz(m5A,m5B)

multMatrizPar(m1A,m1B)
multMatrizPar(m2A,m2B)
multMatrizPar(m3A,m3B)
multMatrizPar(m4A,m4B)
multMatrizPar(m5A,m5B)

multMatrizRec(m1A,m1B)
multMatrizRec(m2A,m2B)
multMatrizRec(m3A,m3B)
multMatrizRec(m4A,m4B)
multMatrizRec(m5A,m5B)

multMatrizRecPar(m1A,m1B)
multMatrizRecPar(m2A,m2B)
multMatrizRecPar(m3A,m3B)
multMatrizRecPar(m4A,m4B)
multMatrizRecPar(m5A,m5B)

multStrassen(m1A,m1B)
multStrassen(m2A,m2B)
multStrassen(m3A,m3B)
multStrassen(m4A,m4B)
multStrassen(m5A,m5B)

multStrassenPar(m1A,m1B)
multStrassenPar(m2A,m2B)
multStrassenPar(m3A,m3B)
multStrassenPar(m4A,m4B)
multStrassenPar(m5A,m5B)








