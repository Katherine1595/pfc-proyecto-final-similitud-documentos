package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Casos de prueba propios del equipo: cinco por función, distintos de las
 * pruebas de contrato del profesor y de los ejemplos del enunciado.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudCasosPropiosTest extends AnyFunSuite {

  // ---------------------------------------------------------------------------
  // normalizar
  // ---------------------------------------------------------------------------

  test("normalizar: la puntuación separa palabras") {
    assert(normalizar("Hola, mundo! ¿Qué tal?") ==
      Vector("hola", "mundo", "qué", "tal"))
  }

  test("normalizar: los espacios repetidos no dejan palabras vacías") {
    assert(normalizar("  uno   dos \t tres\n") == Vector("uno", "dos", "tres"))
  }

  test("normalizar: los dígitos se conservan como parte de las palabras") {
    assert(normalizar("Taller 3: k=4") == Vector("taller", "3", "k", "4"))
  }

  test("normalizar: un guion separa dos palabras") {
    assert(normalizar("paralelismo-de-datos") ==
      Vector("paralelismo", "de", "datos"))
  }

  test("normalizar: una cadena vacía o solo de signos da el vector vacío") {
    assert(normalizar("") == Vector())
    assert(normalizar("...,;!?") == Vector())
  }

  // ---------------------------------------------------------------------------
  // fragmentos
  // ---------------------------------------------------------------------------

  test("fragmentos: con k = 1 cada palabra es un fragmento") {
    assert(fragmentos(Vector("a", "b", "c"), 1) ==
      Vector(Vector("a"), Vector("b"), Vector("c")))
  }

  test("fragmentos: con k igual al largo hay un solo fragmento") {
    assert(fragmentos(Vector("a", "b", "c"), 3) ==
      Vector(Vector("a", "b", "c")))
  }

  test("fragmentos: con menos de k palabras el resultado es vacío") {
    assert(fragmentos(Vector("a", "b"), 3) == Vector())
    assert(fragmentos(Vector(), 2) == Vector())
  }

  test("fragmentos: un vector de t palabras da t - k + 1 fragmentos") {
    val ts = Vector.tabulate(10)(i => "w" + i)
    assert(fragmentos(ts, 4).length == 10 - 4 + 1)
  }

  test("fragmentos: conserva las repeticiones y el orden de aparición") {
    assert(fragmentos(Vector("a", "a", "a", "b"), 2) ==
      Vector(Vector("a", "a"), Vector("a", "a"), Vector("a", "b")))
  }

  // ---------------------------------------------------------------------------
  // huella
  // ---------------------------------------------------------------------------

  test("huella: un fragmento repetido cuenta una sola vez") {
    val d: Documento = ("x", Vector("a", "b", "a", "b"))
    assert(huella(d, 2) == Set(Vector("a", "b"), Vector("b", "a")))
  }

  test("huella: un documento con menos de k palabras tiene huella vacía") {
    val d: Documento = ("x", Vector("a", "b"))
    assert(huella(d, 3) == Set[Fragmento]())
  }

  test("huella: con k = 1 son las palabras distintas") {
    val d: Documento = ("y", Vector("a", "a", "b"))
    assert(huella(d, 1) == Set(Vector("a"), Vector("b")))
  }

  test("huella: sin repeticiones hay t - k + 1 fragmentos") {
    val d: Documento = ("z", Vector("a", "b", "c", "d", "e"))
    assert(huella(d, 3).size == 5 - 3 + 1)
    assert(huella(d, 3).contains(Vector("c", "d", "e")))
  }

  test("huella: un documento de una sola palabra repetida tiene un fragmento") {
    val d: Documento = ("w", Vector.fill(6)("z"))
    assert(huella(d, 2) == Set(Vector("z", "z")))
  }

  // ---------------------------------------------------------------------------
  // huellas
  // ---------------------------------------------------------------------------

  test("huellas: un corpus vacío da un vector vacío") {
    assert(huellas(Vector(), 3) == Vector[Huella]())
  }

  test("huellas: hay una huella por documento") {
    val c: Corpus = Vector(("a", Vector("x", "y", "z")), ("b", Vector("x")),
      ("c", Vector("p", "q")))
    assert(huellas(c, 2).length == 3)
  }

  test("huellas: la posición i es la huella del documento i") {
    val c: Corpus = Vector(("a", Vector("x", "y", "z")), ("b", Vector("x", "y")),
      ("c", Vector("p", "q", "r", "s")))
    assert((0 until c.length).forall(i => huellas(c, 2)(i) == huella(c(i), 2)))
  }

  test("huellas: conserva el orden del corpus") {
    val c: Corpus = Vector(("a", Vector("x", "y")), ("b", Vector("p", "q", "r")))
    assert(huellas(c, 2) ==
      Vector(Set(Vector("x", "y")), Set(Vector("p", "q"), Vector("q", "r"))))
  }

  test("huellas: un documento corto deja su huella vacía en su posición") {
    val c: Corpus = Vector(("a", Vector("x")), ("b", Vector("p", "q")))
    assert(huellas(c, 2) == Vector(Set[Fragmento](), Set(Vector("p", "q"))))
  }

  // ---------------------------------------------------------------------------
  // jaccard
  // ---------------------------------------------------------------------------

  val eps = 1e-9
  val ha: Huella = Set(Vector("a"), Vector("b"), Vector("c"))
  val hb: Huella = Set(Vector("b"), Vector("c"), Vector("d"), Vector("e"))

  test("jaccard: dos huellas iguales dan 1") {
    assert(math.abs(jaccard(ha, ha) - 1.0) < eps)
  }

  test("jaccard: dos huellas sin fragmentos comunes dan 0") {
    assert(jaccard(Set(Vector("a")), Set(Vector("z"))) == 0.0)
  }

  test("jaccard: dos huellas vacías dan 0 y no dividen por cero") {
    assert(jaccard(Set[Fragmento](), Set[Fragmento]()) == 0.0)
  }

  test("jaccard: dos comunes de cinco en la unión dan 0.4, en los dos sentidos") {
    assert(math.abs(jaccard(ha, hb) - 0.4) < eps)
    assert(math.abs(jaccard(hb, ha) - 0.4) < eps)
  }

  test("jaccard: una huella contenida en otra da el cociente de los tamaños") {
    val chica: Huella = Set(Vector("b"), Vector("c"))
    assert(math.abs(jaccard(chica, hb) - 0.5) < eps)
  }

  test("jaccard: una huella vacía contra una no vacía da 0") {
    assert(jaccard(Set[Fragmento](), ha) == 0.0)
  }

  // ---------------------------------------------------------------------------
  // parejas
  // ---------------------------------------------------------------------------

  test("parejas: con 0 o 1 documentos no hay parejas") {
    assert(parejas(0) == Vector[Pareja]())
    assert(parejas(1) == Vector[Pareja]())
  }

  test("parejas: con 2 documentos hay una sola") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas: con 4 documentos salen las seis, en orden") {
    assert(parejas(4) ==
      Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejas: con 10 documentos hay 45") {
    assert(parejas(10).length == 10 * 9 / 2)
  }

  test("parejas: siempre i < j y ninguna se repite") {
    val ps = parejas(8)
    assert(ps.forall { case (i, j) => i < j })
    assert(ps.distinct.length == ps.length)
  }

  // ---------------------------------------------------------------------------
  // similitudes
  // ---------------------------------------------------------------------------

  val hs3: Vector[Huella] = Vector(
    Set(Vector("a"), Vector("b")),
    Set(Vector("b"), Vector("c")),
    Set(Vector("x")))

  test("similitudes: sin parejas no hay mediciones") {
    assert(similitudes(hs3, Vector()) == Vector[Medicion]())
  }

  test("similitudes: una pareja con un fragmento común de tres da 1/3") {
    val ms = similitudes(hs3, Vector((0, 1)))
    assert(ms.length == 1 && ms(0)._1 == 0 && ms(0)._2 == 1)
    assert(math.abs(ms(0)._3 - 1.0 / 3.0) < eps)
  }

  test("similitudes: respeta el orden de las parejas que recibe") {
    val ms = similitudes(hs3, Vector((1, 2), (0, 1)))
    assert(ms.map(m => (m._1, m._2)) == Vector((1, 2), (0, 1)))
    assert(ms(0)._3 == 0.0)
  }

  test("similitudes: con todas las parejas de tres documentos da 1/3, 0 y 0") {
    val ms = similitudes(hs3, parejas(3))
    assert(ms.length == 3)
    assert(math.abs(ms(0)._3 - 1.0 / 3.0) < eps && ms(1)._3 == 0.0 && ms(2)._3 == 0.0)
  }

  test("similitudes: dos documentos con la misma huella dan 1") {
    val iguales: Vector[Huella] = Vector(Set(Vector("a")), Set(Vector("a")))
    assert(similitudes(iguales, Vector((0, 1))).map(_._3) == Vector(1.0))
  }

  test("similitudes: dos huellas vacías dan 0") {
    val vacias: Vector[Huella] = Vector(Set[Fragmento](), Set[Fragmento]())
    assert(similitudes(vacias, Vector((0, 1))).map(_._3) == Vector(0.0))
  }

  // ---------------------------------------------------------------------
  // indiceInvertido
  // ---------------------------------------------------------------------

  val hsIdx: Vector[Huella] = Vector(
    Set(Vector("a"), Vector("b")),
    Set(Vector("b"), Vector("c")),
    Set(Vector("b")))

  test("indiceInvertido: un fragmento en los tres documentos da {0, 1, 2}") {
    assert(indiceInvertido(hsIdx)(Vector("b")) == Set(0, 1, 2))
  }

  test("indiceInvertido: un fragmento en un solo documento da ese documento") {
    assert(indiceInvertido(hsIdx)(Vector("a")) == Set(0))
    assert(indiceInvertido(hsIdx)(Vector("c")) == Set(1))
  }

  test("indiceInvertido: el mapa completo tiene una entrada por fragmento distinto") {
    assert(indiceInvertido(hsIdx) == Map(
      Vector("a") -> Set(0),
      Vector("b") -> Set(0, 1, 2),
      Vector("c") -> Set(1)))
  }

  test("indiceInvertido: un fragmento que no está en ninguna huella no aparece") {
    assert(!indiceInvertido(hsIdx).contains(Vector("z")))
  }

  test("indiceInvertido: sin huellas da un mapa vacío") {
    assert(indiceInvertido(Vector()) == Map())
  }

  test("indiceInvertido: huellas vacías dan un mapa vacío") {
    assert(indiceInvertido(Vector(Set[Fragmento](), Set[Fragmento]())) == Map())
  }

  // ---------------------------------------------------------------------
  // parejasCandidatas
  // ---------------------------------------------------------------------

  test("parejasCandidatas: un fragmento en dos documentos da una pareja") {
    assert(parejasCandidatas(Map(Vector("a") -> Set(0, 1))) == Vector((0, 1)))
  }

  test("parejasCandidatas: un fragmento en tres documentos da las tres parejas") {
    assert(parejasCandidatas(Map(Vector("a") -> Set(0, 1, 2))) ==
      Vector((0, 1), (0, 2), (1, 2)))
  }

  test("parejasCandidatas: una pareja que sale de dos entradas no se repite") {
    val idx: Indice = Map(Vector("a") -> Set(0, 1), Vector("b") -> Set(0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1)))
  }

  test("parejasCandidatas: un fragmento en un solo documento no genera parejas") {
    assert(parejasCandidatas(Map(Vector("a") -> Set(2))) == Vector())
  }

  test("parejasCandidatas: el índice vacío no genera parejas") {
    assert(parejasCandidatas(Map()) == Vector())
  }

  test("parejasCandidatas: salen ordenadas aunque las entradas no lo estén") {
    val idx: Indice = Map(Vector("a") -> Set(1, 2), Vector("b") -> Set(0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1), (1, 2)))
  }

  test("parejasCandidatas: documentos no consecutivos también forman pareja") {
    assert(parejasCandidatas(Map(Vector("a") -> Set(0, 3))) == Vector((0, 3)))
  }

  // ---------------------------------------------------------------------
  // ranking
  // ---------------------------------------------------------------------

  val msR: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))

  test("ranking: el umbral es inclusivo (>=)") {
    assert(ranking(msR, 0.4, 10) == Vector((1, 2, 0.9), (0, 1, 0.4)))
  }

  test("ranking: un umbral mayor que todas las similitudes da vacío") {
    assert(ranking(msR, 0.95, 10) == Vector())
  }

  test("ranking: con p = 0 no devuelve nada") {
    assert(ranking(msR, 0.0, 0) == Vector())
  }

  test("ranking: si p es mayor que lo que hay, devuelve todo") {
    assert(ranking(msR, 0.0, 100).length == 3)
  }

  test("ranking: los empates se rompen por posición (i, j)") {
    val empatadas: Vector[Medicion] = Vector((1, 2, 0.5), (0, 2, 0.5), (0, 1, 0.5))
    assert(ranking(empatadas, 0.0, 10) ==
      Vector((0, 1, 0.5), (0, 2, 0.5), (1, 2, 0.5)))
  }

  test("ranking: el resultado no depende del orden de entrada") {
    assert(ranking(msR.reverse, 0.0, 10) == ranking(msR, 0.0, 10))
  }

  test("ranking: sin mediciones da vacío") {
    assert(ranking(Vector(), 0.0, 10) == Vector())
  }

  test("ranking: con muchas mediciones queda ordenado y no desborda la pila") {
    val grande: Vector[Medicion] =
      (0 until 20000).toVector.map(i => (i, i + 1, (i % 7) / 10.0))
    val r = ranking(grande, 0.0, 20000)
    assert(r.length == 20000)
    assert(r.head._3 == 0.6 && r.last._3 == 0.0)
  }

  // ---------------------------------------------------------------------
  // detectarDirecto y detectarConIndice
  // ---------------------------------------------------------------------

  // a y b comparten 2 de 4 fragmentos de tamaño 2 (similitud 0.5); c no
  // comparte nada con ninguno.
  val ca: Corpus = Vector(
    ("a", Vector("x", "y", "z", "w")),
    ("b", Vector("x", "y", "z", "q")),
    ("c", Vector("m", "n", "o", "p")))

  // a y c son idénticos (similitud 1); b se parece a los dos con 0.5.
  val cb: Corpus = Vector(
    ("a", Vector("x", "y", "z", "w")),
    ("b", Vector("x", "y", "z", "q")),
    ("c", Vector("x", "y", "z", "w")))

  test("detectarDirecto: solo la pareja similar supera el umbral") {
    val r = detectarDirecto(ca, 2, 0.1, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(r(0)._3 - 0.5) < eps)
  }

  test("detectarDirecto: un umbral alto no deja ninguna pareja") {
    assert(detectarDirecto(ca, 2, 0.6, 5) == Vector())
  }

  test("detectarDirecto: con umbral 0 entran todas las parejas, la más similar primero") {
    val r = detectarDirecto(ca, 2, 0.0, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
  }

  test("detectarDirecto: p = 0 no devuelve nada") {
    assert(detectarDirecto(ca, 2, 0.1, 0) == Vector())
  }

  test("detectarDirecto: un corpus vacío o de un documento no tiene parejas") {
    assert(detectarDirecto(Vector(), 2, 0.1, 5) == Vector())
    assert(detectarDirecto(Vector(ca(0)), 2, 0.1, 5) == Vector())
  }

  test("detectarDirecto: ordena por similitud y desempata por posición") {
    val r = detectarDirecto(cb, 2, 0.1, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 2), (0, 1), (1, 2)))
    assert(math.abs(r(0)._3 - 1.0) < eps)
  }

  test("detectarConIndice: solo la pareja similar supera el umbral") {
    val r = detectarConIndice(ca, 2, 0.1, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(math.abs(r(0)._3 - 0.5) < eps)
  }

  test("detectarConIndice: un umbral alto no deja ninguna pareja") {
    assert(detectarConIndice(ca, 2, 0.6, 5) == Vector())
  }

  test("detectarConIndice: p = 0 no devuelve nada") {
    assert(detectarConIndice(ca, 2, 0.1, 0) == Vector())
  }

  test("detectarConIndice: un corpus vacío o de un documento no tiene parejas") {
    assert(detectarConIndice(Vector(), 2, 0.1, 5) == Vector())
    assert(detectarConIndice(Vector(ca(0)), 2, 0.1, 5) == Vector())
  }

  test("detectarConIndice: ordena por similitud y desempata por posición") {
    val r = detectarConIndice(cb, 2, 0.1, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 2), (0, 1), (1, 2)))
  }

  test("detectarConIndice: da lo mismo que el directo cuando el umbral es positivo") {
    assert(detectarConIndice(ca, 2, 0.1, 5) == detectarDirecto(ca, 2, 0.1, 5))
    assert(detectarConIndice(cb, 2, 0.1, 5) == detectarDirecto(cb, 2, 0.1, 5))
  }

  test("detectarConIndice: con umbral 0 no ve las parejas sin fragmentos comunes") {
    assert(detectarConIndice(ca, 2, 0.0, 5).map(m => (m._1, m._2)) == Vector((0, 1)))
    assert(detectarDirecto(ca, 2, 0.0, 5).length == 3)
  }
}
