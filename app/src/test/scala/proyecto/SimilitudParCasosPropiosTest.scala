package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._
import SimilitudPar._

/**
 * Casos de prueba propios del equipo para la versión paralela: cinco por
 * función o más, distintos de las pruebas de contrato del profesor.
 *
 * La condición que se verifica es la del enunciado: cada versión paralela da
 * exactamente lo mismo que su versión secuencial. Los corpus al azar cambian en
 * cada corrida, y la igualdad tiene que cumplirse siempre.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudParCasosPropiosTest extends AnyFunSuite {

  val cortes: Vector[Int] = Vector(1, 4, 16, 64, 1000)

  // ---------------------------------------------------------------------------
  // huellasPar
  // ---------------------------------------------------------------------------

  test("huellasPar: un corpus vacío da un vector vacío") {
    assert(huellasPar(Vector(), 3) == Vector())
  }

  test("huellasPar: un corpus de un solo documento") {
    val c: Corpus = Vector(("a", Vector("x", "y", "z", "w")))
    assert(huellasPar(c, 2) == huellas(c, 2))
    assert(huellasPar(c, 2).length == 1)
  }

  test("huellasPar: con un número impar de documentos las mitades no son iguales") {
    val c = corpusAlAzar(7, 80, 12)
    assert(huellasPar(c, 3) == huellas(c, 3))
    assert(huellasPar(c, 3).length == 7)
  }

  test("huellasPar: corpus grande al azar da lo mismo que la secuencial") {
    val c = corpusAlAzar(120, 200, 30)
    assert(huellasPar(c, 4) == huellas(c, 4))
  }

  test("huellasPar: mantiene las huellas vacías de los documentos cortos en su posición") {
    val c: Corpus = Vector(
      ("a", Vector("x", "y", "z")),
      ("b", Vector("x")),
      ("c", Vector("x", "y", "z", "w")),
      ("d", Vector()),
      ("e", Vector("x", "y", "z")))
    val hs = huellasPar(c, 3)
    assert(hs == huellas(c, 3))
    assert(hs(1).isEmpty && hs(3).isEmpty && hs(2).size == 2)
  }

  test("huellasPar: documentos idénticos tienen huellas idénticas en cualquier mitad") {
    val d: Documento = ("a", Vector("p1", "p2", "p3", "p4", "p5", "p6"))
    val c: Corpus = Vector.fill(9)(d)
    val hs = huellasPar(c, 3)
    assert(hs.forall(h => h == hs(0)) && hs(0).size == 4)
  }

  // ---------------------------------------------------------------------------
  // similitudesPar
  // ---------------------------------------------------------------------------

  test("similitudesPar: sin parejas no hay mediciones, con cualquier corte") {
    val hs = huellas(corpusAlAzar(5, 50, 10), 3)
    assert(cortes.forall(corte => similitudesPar(hs, Vector(), corte) == Vector()))
  }

  test("similitudesPar: una sola pareja da lo mismo con corte 1 y con corte 0") {
    val hs = huellas(corpusAlAzar(4, 60, 10), 3)
    val ps: Vector[Pareja] = Vector((1, 3))
    assert(similitudesPar(hs, ps, 1) == similitudes(hs, ps))
    assert(similitudesPar(hs, ps, 0) == similitudes(hs, ps))
  }

  test("similitudesPar: un corte menor que 1 no hace que la recursión se repita") {
    val hs = huellas(corpusAlAzar(6, 60, 10), 3)
    val ps = parejas(6)
    assert(similitudesPar(hs, ps, 0) == similitudes(hs, ps))
    assert(similitudesPar(hs, ps, -5) == similitudes(hs, ps))
  }

  test("similitudesPar: un corte mayor que las parejas se resuelve sin partir") {
    val hs = huellas(corpusAlAzar(8, 60, 10), 3)
    val ps = parejas(8)
    assert(similitudesPar(hs, ps, 10000) == similitudes(hs, ps))
  }

  test("similitudesPar: con varios cortes conserva valores y orden de las parejas") {
    val hs = huellas(corpusAlAzar(25, 100, 15), 3)
    val ps = parejas(25)
    assert(cortes.forall(corte => similitudesPar(hs, ps, corte) == similitudes(hs, ps)))
    assert(similitudesPar(hs, ps, 7).map(m => (m._1, m._2)) == ps)
  }

  test("similitudesPar: un número impar de parejas también da lo mismo") {
    val hs = huellas(corpusAlAzar(10, 80, 12), 3)
    val ps = parejas(10).take(37)
    assert(cortes.forall(corte => similitudesPar(hs, ps, corte) == similitudes(hs, ps)))
  }

  test("similitudesPar: funciona con las parejas candidatas, que no son todas") {
    val hs = huellas(corpusAlAzar(30, 100, 20), 4)
    val cand = parejasCandidatas(indiceInvertido(hs))
    assert(cand.length < parejas(30).length)
    assert(similitudesPar(hs, cand, 5) == similitudes(hs, cand))
  }

  // ---------------------------------------------------------------------------
  // fusionar
  // ---------------------------------------------------------------------------

  val f1: Fragmento = Vector("a", "b")
  val f2: Fragmento = Vector("c", "d")
  val f3: Fragmento = Vector("e", "f")
  val ia: Indice = Map(f1 -> Set(0, 1), f2 -> Set(2))
  val ib: Indice = Map(f1 -> Set(5), f3 -> Set(3, 4))
  val ic: Indice = Map(f2 -> Set(9), f3 -> Set(7))

  test("fusionar: con un índice vacío a cada lado devuelve el otro") {
    assert(fusionar(ia, Map()) == ia)
    assert(fusionar(Map(), ia) == ia)
    assert(fusionar(Map(), Map()) == Map())
  }

  test("fusionar: si no hay fragmentos comunes es la unión de los dos mapas") {
    val i1: Indice = Map(f1 -> Set(0))
    val i2: Indice = Map(f2 -> Set(1), f3 -> Set(2))
    assert(fusionar(i1, i2) == Map(f1 -> Set(0), f2 -> Set(1), f3 -> Set(2)))
  }

  test("fusionar: un fragmento común reúne los documentos de los dos") {
    assert(fusionar(ia, ib) ==
      Map(f1 -> Set(0, 1, 5), f2 -> Set(2), f3 -> Set(3, 4)))
  }

  test("fusionar: es conmutativa") {
    assert(fusionar(ia, ib) == fusionar(ib, ia))
    assert(fusionar(ib, ic) == fusionar(ic, ib))
  }

  test("fusionar: es asociativa, y por eso el orden de las fusiones no importa") {
    assert(fusionar(fusionar(ia, ib), ic) == fusionar(ia, fusionar(ib, ic)))
  }

  test("fusionar: fusionar un índice consigo mismo lo deja igual") {
    assert(fusionar(ia, ia) == ia)
  }

  test("fusionar: no modifica los índices que recibe") {
    val copiaA = ia
    val copiaB = ib
    val r = fusionar(ia, ib)
    assert(ia == copiaA && ib == copiaB && r != ia)
  }

  // ---------------------------------------------------------------------------
  // indiceInvertidoPar
  // ---------------------------------------------------------------------------

  test("indiceInvertidoPar: sin huellas da un mapa vacío") {
    assert(indiceInvertidoPar(Vector()) == Map())
  }

  test("indiceInvertidoPar: con una sola huella no desplaza nada") {
    val hs: Vector[Huella] = Vector(Set(Vector("a"), Vector("b")))
    assert(indiceInvertidoPar(hs) == Map(Vector("a") -> Set(0), Vector("b") -> Set(0)))
  }

  test("indiceInvertidoPar: dos documentos con el mismo fragmento quedan en {0, 1}") {
    val hs: Vector[Huella] = Vector(Set(Vector("a")), Set(Vector("a")))
    assert(indiceInvertidoPar(hs) == Map(Vector("a") -> Set(0, 1)))
  }

  test("indiceInvertidoPar: un fragmento solo de la mitad derecha conserva su posición") {
    val hs: Vector[Huella] = Vector(
      Set(Vector("a")), Set(Vector("a")), Set(Vector("a")),
      Set(Vector("a")), Set(Vector("a")), Set(Vector("z")))
    val idx = indiceInvertidoPar(hs)
    assert(idx(Vector("z")) == Set(5))
    assert(idx(Vector("a")) == Set(0, 1, 2, 3, 4))
  }

  test("indiceInvertidoPar: con un número impar de documentos da lo mismo que la secuencial") {
    val hs = huellas(corpusAlAzar(9, 80, 12), 3)
    assert(indiceInvertidoPar(hs) == indiceInvertido(hs))
  }

  test("indiceInvertidoPar: corpus grande al azar da lo mismo que la secuencial") {
    val hs = huellas(corpusAlAzar(100, 150, 25), 3)
    assert(indiceInvertidoPar(hs) == indiceInvertido(hs))
  }

  test("indiceInvertidoPar: huellas vacías mezcladas con huellas llenas") {
    val hs: Vector[Huella] = Vector(
      Set[Fragmento](), Set(Vector("a")), Set[Fragmento](),
      Set(Vector("a"), Vector("b")), Set[Fragmento]())
    assert(indiceInvertidoPar(hs) == indiceInvertido(hs))
    assert(indiceInvertidoPar(hs)(Vector("a")) == Set(1, 3))
  }

  test("indiceInvertidoPar: ningún documento queda con una posición fuera de rango") {
    val hs = huellas(corpusAlAzar(33, 60, 10), 3)
    val todas = indiceInvertidoPar(hs).values.toList.flatMap(ds => ds.toList)
    assert(todas.forall(d => d >= 0 && d < 33))
  }

  // ---------------------------------------------------------------------------
  // detectarDirectoPar
  // ---------------------------------------------------------------------------

  val ca: Corpus = Vector(
    ("a", Vector("x", "y", "z", "w")),
    ("b", Vector("x", "y", "z", "q")),
    ("c", Vector("m", "n", "o", "p")))

  test("detectarDirectoPar: da lo mismo que la secuencial con varios cortes") {
    val c = corpusAlAzar(40, 120, 15)
    val esperado = detectarDirecto(c, 3, 0.05, 20)
    assert(cortes.forall(corte => detectarDirectoPar(c, 3, 0.05, 20, corte) == esperado))
  }

  test("detectarDirectoPar: encuentra las copias parciales, que quedan de primeras") {
    val n = 30
    val c = corpusConCopias(n, 200, 1000, 3, 0.8)
    val r = detectarDirectoPar(c, 3, 0.1, 3, 16)
    assert(r.map(m => (m._1, m._2)).toSet == Set((0, n), (1, n + 1), (2, n + 2)))
  }

  test("detectarDirectoPar: la similitud de una copia es el cociente esperado") {
    // La copia toma los primeros 160 de 200 palabras: comparte 158 fragmentos de
    // los 198 de cada documento, y la unión es 198 + 198 - 158 = 238.
    val c = corpusConCopias(10, 200, 5000, 1, 0.8)
    val r = detectarDirectoPar(c, 3, 0.1, 1, 8)
    assert(math.abs(r(0)._3 - 158.0 / 238.0) < 1e-9)
  }

  test("detectarDirectoPar: un umbral mayor que todas las similitudes da vacío") {
    assert(detectarDirectoPar(ca, 2, 0.6, 5, 2) == Vector())
  }

  test("detectarDirectoPar: p = 0 no devuelve nada y p grande devuelve todo lo que pasa") {
    assert(detectarDirectoPar(ca, 2, 0.1, 0, 2) == Vector())
    assert(detectarDirectoPar(ca, 2, 0.0, 100, 2).length == 3)
  }

  test("detectarDirectoPar: un corpus vacío o de un documento no tiene parejas") {
    assert(detectarDirectoPar(Vector(), 2, 0.1, 5, 4) == Vector())
    assert(detectarDirectoPar(Vector(ca(0)), 2, 0.1, 5, 4) == Vector())
  }

  test("detectarDirectoPar: p trunca el mismo ranking que p grande") {
    val c = corpusConCopias(20, 150, 800, 5, 0.7)
    val completo = detectarDirectoPar(c, 3, 0.05, 50, 16)
    assert(detectarDirectoPar(c, 3, 0.05, 2, 16) == completo.take(2))
  }

  // ---------------------------------------------------------------------------
  // detectarConIndicePar
  // ---------------------------------------------------------------------------

  test("detectarConIndicePar: da lo mismo que la secuencial con varios cortes") {
    val c = corpusAlAzar(40, 120, 15)
    val esperado = detectarConIndice(c, 3, 0.05, 20)
    assert(cortes.forall(corte => detectarConIndicePar(c, 3, 0.05, 20, corte) == esperado))
  }

  test("detectarConIndicePar: encuentra las copias parciales, que quedan de primeras") {
    val n = 30
    val c = corpusConCopias(n, 200, 1000, 3, 0.8)
    val r = detectarConIndicePar(c, 3, 0.1, 3, 16)
    assert(r.map(m => (m._1, m._2)).toSet == Set((0, n), (1, n + 1), (2, n + 2)))
  }

  test("detectarConIndicePar: las cuatro soluciones coinciden con umbral positivo") {
    val c = corpusAlAzar(35, 100, 18)
    val directo = detectarDirecto(c, 3, 0.02, 25)
    assert(detectarConIndice(c, 3, 0.02, 25) == directo)
    assert(detectarDirectoPar(c, 3, 0.02, 25, 32) == directo)
    assert(detectarConIndicePar(c, 3, 0.02, 25, 32) == directo)
  }

  test("detectarConIndicePar: con umbral 0 no ve las parejas sin fragmentos comunes") {
    assert(detectarConIndicePar(ca, 2, 0.0, 5, 2).map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(detectarDirectoPar(ca, 2, 0.0, 5, 2).length == 3)
  }

  test("detectarConIndicePar: un corpus vacío o de un documento no tiene parejas") {
    assert(detectarConIndicePar(Vector(), 2, 0.1, 5, 4) == Vector())
    assert(detectarConIndicePar(Vector(ca(0)), 2, 0.1, 5, 4) == Vector())
  }

  test("detectarConIndicePar: un umbral alto o p = 0 no dejan nada") {
    assert(detectarConIndicePar(ca, 2, 0.6, 5, 2) == Vector())
    assert(detectarConIndicePar(ca, 2, 0.1, 0, 2) == Vector())
  }

  test("detectarConIndicePar: con muchos documentos y poco vocabulario sobreviven casi todas") {
    // Con v = 6 y k = 2 casi todas las parejas comparten un fragmento: el índice
    // casi no descarta nada y el resultado tiene que seguir siendo el mismo.
    val c = corpusAlAzar(25, 60, 6)
    assert(detectarConIndicePar(c, 2, 0.05, 30, 10) == detectarDirecto(c, 2, 0.05, 30))
  }
}
