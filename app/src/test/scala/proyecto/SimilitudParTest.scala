package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._
import SimilitudPar._

/**
 * Pruebas de contrato de la versión paralela: cada una compara contra la
 * versión secuencial, que es la condición que pide el enunciado. No se
 * modifican.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudParTest extends AnyFunSuite {

  val c1: Corpus = Vector(
    ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa")),
    ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra")),
    ("d2", Vector("la", "lluvia", "moja", "el", "parque")))

  val c2: Corpus = Vector(
    ("e0", Vector("la", "funcion", "recursiva", "termina", "cuando", "llega",
      "al", "caso", "base")),
    ("e1", Vector("toda", "funcion", "recursiva", "termina", "cuando", "llega",
      "al", "caso", "base")),
    ("e2", Vector("el", "paralelismo", "divide", "el", "trabajo", "en",
      "tareas", "independientes")),
    ("e3", Vector("el", "paralelismo", "divide", "el", "trabajo", "en",
      "hilos", "independientes")))

  test("huellasPar: da lo mismo que la secuencial") {
    assert(huellasPar(c1, 3) == huellas(c1, 3))
    assert(huellasPar(c2, 3) == huellas(c2, 3))
  }

  test("similitudesPar: da lo mismo que la secuencial, y conserva el orden") {
    val hs = huellas(c2, 3)
    val ps = parejas(c2.length)
    assert(similitudesPar(hs, ps, 1) == similitudes(hs, ps))
    assert(similitudesPar(hs, ps, 100) == similitudes(hs, ps))
  }

  test("fusionar: une los conjuntos del fragmento que está en los dos") {
    val g: Fragmento = Vector("a", "b")
    val h: Fragmento = Vector("c", "d")
    val i1: Indice = Map(g -> Set(0), h -> Set(1))
    val i2: Indice = Map(g -> Set(2))
    assert(fusionar(i1, i2) == Map(g -> Set(0, 2), h -> Set(1)))
    assert(fusionar(i1, Map()) == i1)
  }

  test("indiceInvertidoPar: da lo mismo que la secuencial en los ejemplos") {
    assert(indiceInvertidoPar(huellas(c1, 3)) == indiceInvertido(huellas(c1, 3)))
    assert(indiceInvertidoPar(huellas(c2, 3)) == indiceInvertido(huellas(c2, 3)))
  }

  test("detectar: las cuatro versiones coinciden") {
    val directo = detectarDirecto(c2, 3, 0.3, 5)
    assert(detectarConIndice(c2, 3, 0.3, 5) == directo)
    assert(detectarDirectoPar(c2, 3, 0.3, 5, 2) == directo)
    assert(detectarConIndicePar(c2, 3, 0.3, 5, 2) == directo)
  }
}
