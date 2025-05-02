import Matrices._

val matriz = Vector(Vector(0, 2, 2), Vector(2, 2, 0), Vector(0, 2, 2))
val matriz2 = Vector(Vector(1, 1, 1), Vector(1, 0, 1), Vector(2, 0, 2))

val matriz44 = matrizAlAzar(6,8)
val matriz442 = matrizAlAzar(6,2)



multMatriz(matriz, matriz2)
multMatrizPar(matriz,matriz2)
multMatrizPar(matriz44,matriz442)
multMatriz(matriz44, matriz442)

subMatriz(matriz44, 2, 2 , matriz44.length/2)
sumMatriz(matriz,matriz2)




