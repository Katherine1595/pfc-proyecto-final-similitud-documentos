package proyecto

import Similitud._
import SimilitudPar._

/**
 * Programa de arranque: los dos ejemplos del enunciado.
 *
 * Corre con `./gradlew run`. Mientras las funciones digan `???` falla en la
 * primera que se use, que es lo esperado al clonar.
 */
object App {

  val ejemplo1: Corpus = Vector(
    ("d0", normalizar("el gato duerme en el sofa")),
    ("d1", normalizar("el gato duerme en la alfombra")),
    ("d2", normalizar("la lluvia moja el parque")))

  val ejemplo2: Corpus = Vector(
    ("e0", normalizar("la funcion recursiva termina cuando llega al caso base")),
    ("e1", normalizar("toda funcion recursiva termina cuando llega al caso base")),
    ("e2", normalizar("el paralelismo divide el trabajo en tareas independientes")),
    ("e3", normalizar("el paralelismo divide el trabajo en hilos independientes")))

  def main(args: Array[String]): Unit = {
    println("Ejemplo 1, k = 3, umbral = 0.1")
    println("  directo:    " + detectarDirecto(ejemplo1, 3, 0.1, 10))
    println("  con indice: " + detectarConIndice(ejemplo1, 3, 0.1, 10))

    println("Ejemplo 2, k = 3, umbral = 0.1")
    println("  directo:    " + detectarDirecto(ejemplo2, 3, 0.1, 10))
    println("  con indice: " + detectarConIndice(ejemplo2, 3, 0.1, 10))

    println("Las versiones paralelas, con corte 2")
    println("  directo:    " + detectarDirectoPar(ejemplo2, 3, 0.1, 10, 2))
    println("  con indice: " + detectarConIndicePar(ejemplo2, 3, 0.1, 10, 2))
  }
}
