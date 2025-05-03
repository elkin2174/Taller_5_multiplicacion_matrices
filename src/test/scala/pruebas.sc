import Matrices.*


val matriz = matrizAlAzar(math.pow(2,5).toInt,2)
val matriz2 = matrizAlAzar(math.pow(2,5).toInt,2)

val matriz1 = Vector(Vector(1,2),Vector(1,0))
val matriz11 = Vector(Vector(1,0),Vector(1,1))



multMatriz(matriz, matriz2)
multMatrizPar(matriz,matriz2)
multMatrizRec(matriz,matriz2)
multMatrizRecPar(matriz,matriz2)

restaMatriz(matriz1,matriz11)





