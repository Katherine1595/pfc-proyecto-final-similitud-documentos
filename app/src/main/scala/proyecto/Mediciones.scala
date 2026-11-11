package proyecto

import Similitud._
import SimilitudPar._

// Mediciones del informe de paralelización. Es un programa aparte de `App`, que
// se deja como lo entregó el profesor.
//
// Imprime las tablas ya en Markdown. Tarda unos minutos. Se corre así, desde la
// raíz del proyecto:
//
//   ./gradlew installDist
//   java -cp "app/build/install/app/lib/*" proyecto.Mediciones
//
// Más rápido, con menos repeticiones:
//
//   java -Drapido=true -cp "app/build/install/app/lib/*" proyecto.Mediciones
object Mediciones {

  def main(args: Array[String]): Unit = mediciones()

  // ---------------------------------------------------------------------------
  // Medición de tiempos
  //
  // El proyecto no trae ScalaMeter y los archivos de Gradle no se pueden
  // editar, así que se mide como en clase, con System.nanoTime. Para que el
  // compilador de la JVM ya haya optimizado el código antes de medir, cada
  // medición descarta unas ejecuciones de calentamiento y promedia las
  // siguientes.
  // ---------------------------------------------------------------------------

  // Con -Drapido=true se mide menos veces: tarda menos y es más ruidoso.
  val rapido = sys.props.contains("rapido")
  val calentamiento = if (rapido) 2 else 3
  val repeticiones = if (rapido) 3 else 5

  /** Milisegundos que tarda una ejecución del cuerpo. */
  def tiempo[A](cuerpo: => A): Double = {
    val t0 = System.nanoTime()
    val resultado = cuerpo
    val t1 = System.nanoTime()
    (t1 - t0) / 1e6
  }

  /** Promedio en milisegundos, después de las ejecuciones de calentamiento. */
  def medir[A](cuerpo: => A): Double = {
    val descartadas = (0 until calentamiento).map(_ => tiempo(cuerpo))
    val tiempos = (0 until repeticiones).map(_ => tiempo(cuerpo))
    tiempos.sum / repeticiones
  }

  // Valores de partida que sugiere el enunciado.
  val t = 500
  val v = 40
  val k = 4
  val umbral = 0.05
  val p = 20
  val corte = 128

  /** Mensaje de avance por la salida de errores, para no mezclarlo con las tablas. */
  def avance(mensaje: String): Unit = System.err.println("[avance] " + mensaje)

  def f1(x: Double): String = "%.1f".formatLocal(java.util.Locale.US, x)

  /** Aceleración en porcentaje, como en las tablas del enunciado. */
  def aceleracion(sec: Double, par: Double): Double = (sec - par) / sec * 100

  /** Cuántas veces más rápida es la paralela. */
  def veces(sec: Double, par: Double): Double = sec / par

  /** Todas las mediciones de un corpus de n documentos de t palabras. */
  case class Fila(n: Int, t: Int, totales: Int, candidatas: Int,
                  huellasS: Double, huellasP: Double,
                  simS: Double, simP: Double,
                  indiceS: Double, indiceP: Double,
                  directoS: Double, directoP: Double,
                  conIndiceS: Double, conIndiceP: Double)

  def fila(n: Int, tt: Int): Fila = {
    val c = corpusAlAzar(n, tt, v)
    val hs = huellas(c, k)
    val ps = parejas(n)
    val candidatas = parejasCandidatas(indiceInvertido(hs))
    Fila(n, tt, ps.length, candidatas.length,
      medir(huellas(c, k)), medir(huellasPar(c, k)),
      medir(similitudes(hs, ps)), medir(similitudesPar(hs, ps, corte)),
      medir(indiceInvertido(hs)), medir(indiceInvertidoPar(hs)),
      medir(detectarDirecto(c, k, umbral, p)),
      medir(detectarDirectoPar(c, k, umbral, p, corte)),
      medir(detectarConIndice(c, k, umbral, p)),
      medir(detectarConIndicePar(c, k, umbral, p, corte)))
  }

  /** Fracción de las parejas que sobreviven al índice, en porcentaje. */
  def supervivencia(vocabulario: Int, tamano: Int): Double = {
    val n = 100
    val hs = huellas(corpusAlAzar(n, t, vocabulario), tamano)
    100.0 * parejasCandidatas(indiceInvertido(hs)).length / parejas(n).length
  }

  def mediciones(): Unit = {
    val nucleos = Runtime.getRuntime.availableProcessors()
    println("# Resultados de las mediciones")
    println()
    println(s"Núcleos disponibles: $nucleos. t = $t, v = $v, k = $k, " +
      s"umbral = $umbral, p = $p, corte = $corte. Cada tiempo es el promedio de " +
      s"$repeticiones ejecuciones, después de $calentamiento de calentamiento.")
    println()

    // Tabla 0: fracción de parejas que sobreviven al índice, con n = 100.
    avance("midiendo la fracción de supervivencia")
    val vocabularios = Vector(20, 40, 100, 400)
    val tamanos = Vector(3, 4, 5)
    println("## Fracción de parejas que sobreviven al índice (%), n = 100, t = " + t)
    println()
    println("| v \\ k | " + tamanos.map(x => "k = " + x).mkString(" | ") + " |")
    println("|---|" + tamanos.map(x => "---|").mkString)
    println(vocabularios.map(vv =>
      "| v = " + vv + " | " +
        tamanos.map(kk => f1(supervivencia(vv, kk))).mkString(" | ") + " |")
      .mkString("\n"))
    println()

    // Una medición completa por cada tamaño de corpus, con t = 500.
    val tamanosN = Vector(50, 100, 200, 400)
    val filas = tamanosN.map { n =>
      avance(s"midiendo n = $n (de 50, 100, 200 y 400)")
      fila(n, t)
    }

    println("## Tabla 1: solución directa, secuencial contra paralela")
    println()
    println("| Docs n | Secuencial (ms) | Paralela (ms) | Aceleración (%) | Veces |")
    println("|---|---|---|---|---|")
    println(filas.map(f =>
      s"| ${f.n} | ${f1(f.directoS)} | ${f1(f.directoP)} | " +
        s"${f1(aceleracion(f.directoS, f.directoP))} | " +
        s"${f1(veces(f.directoS, f.directoP))} |").mkString("\n"))
    println()

    println("## Tabla 2: directo contra con índice, ambos secuenciales")
    println()
    println("| Docs n | Parejas totales | Candidatas | Directo (ms) | Con índice (ms) |")
    println("|---|---|---|---|---|")
    println(filas.map(f =>
      s"| ${f.n} | ${f.totales} | ${f.candidatas} | " +
        s"${f1(f.directoS)} | ${f1(f.conIndiceS)} |").mkString("\n"))
    println()

    println("## Tabla 3: solución con índice, secuencial contra paralela")
    println()
    println("| Docs n | Secuencial (ms) | Paralela (ms) | Aceleración (%) | Veces |")
    println("|---|---|---|---|---|")
    println(filas.map(f =>
      s"| ${f.n} | ${f1(f.conIndiceS)} | ${f1(f.conIndiceP)} | " +
        s"${f1(aceleracion(f.conIndiceS, f.conIndiceP))} | " +
        s"${f1(veces(f.conIndiceS, f.conIndiceP))} |").mkString("\n"))
    println()

    println("## Tabla 4: cada etapa, secuencial contra paralela (ms)")
    println()
    println("| Docs n | huellas | huellasPar | similitudes | similitudesPar | " +
      "indiceInvertido | indiceInvertidoPar |")
    println("|---|---|---|---|---|---|---|")
    println(filas.map(f =>
      s"| ${f.n} | ${f1(f.huellasS)} | ${f1(f.huellasP)} | ${f1(f.simS)} | " +
        s"${f1(f.simP)} | ${f1(f.indiceS)} | ${f1(f.indiceP)} |").mkString("\n"))
    println()

    // Tabla 5: el efecto del largo t de los documentos, con n = 200.
    val largos = Vector(300, 500, 1000)
    val filasT = largos.map { tt =>
      avance(s"midiendo t = $tt con n = 200 (de 300, 500 y 1000)")
      fila(200, tt)
    }
    println("## Tabla 5: efecto del largo t, con n = 200 (ms)")
    println()
    println("| t | Directo | Directo par. | Veces | Con índice | Con índice par. | Veces |")
    println("|---|---|---|---|---|---|---|")
    println(filasT.map(f =>
      s"| ${f.t} | ${f1(f.directoS)} | ${f1(f.directoP)} | " +
        s"${f1(veces(f.directoS, f.directoP))} | ${f1(f.conIndiceS)} | " +
        s"${f1(f.conIndiceP)} | ${f1(veces(f.conIndiceS, f.conIndiceP))} |")
      .mkString("\n"))
    println()

    // Tabla 6: el valor de corte, con n = 200 y valores separados por 4.
    val c200 = corpusAlAzar(200, t, v)
    avance("midiendo el efecto del corte")
    val cortes = Vector(4, 16, 64, 256, 1024, 4096, 16384)
    println("## Tabla 6: efecto del corte, con n = " + 200 + " y t = " + t + " (ms)")
    println()
    println("| corte | Directo par. | Con índice par. |")
    println("|---|---|---|")
    println(cortes.map(cc =>
      s"| $cc | ${f1(medir(detectarDirectoPar(c200, k, umbral, p, cc)))} | " +
        s"${f1(medir(detectarConIndicePar(c200, k, umbral, p, cc)))} |")
      .mkString("\n"))
    println(s"(Referencia secuencial con n = 200: directo " +
      s"${f1(medir(detectarDirecto(c200, k, umbral, p)))} ms, con índice " +
      s"${f1(medir(detectarConIndice(c200, k, umbral, p)))} ms.)")
    println()

    // Tabla 7: en qué etapa se va el tiempo de la solución con índice.
    val c400 = corpusAlAzar(400, t, v)
    avance("midiendo las etapas y el cálculo de Amdahl (lo último)")
    val hs400 = huellas(c400, k)
    val idx400 = indiceInvertido(hs400)
    val cand400 = parejasCandidatas(idx400)
    val ms400 = similitudes(hs400, cand400)
    val tHuellas = medir(huellas(c400, k))
    val tIndice = medir(indiceInvertido(hs400))
    val tCandidatas = medir(parejasCandidatas(idx400))
    val tSimilitudes = medir(similitudes(hs400, cand400))
    val tRanking = medir(ranking(ms400, umbral, p))
    val total = tHuellas + tIndice + tCandidatas + tSimilitudes + tRanking

    // La parte de indiceInvertidoPar que no se reparte: desplazar los
    // documentos de la mitad derecha y fusionar los dos índices.
    val m = hs400.length / 2
    val iIzq = indiceInvertido(hs400.take(m))
    val iDer = indiceInvertido(hs400.drop(m))
    val desplazar: Indice = iDer.map { case (g, ds) => (g, ds.map(d => d + m)) }
    val tDesplazar = medir(iDer.map { case (g, ds) => (g, ds.map(d => d + m)) })
    val tFusionar = medir(fusionar(iIzq, desplazar))
    val extra = tDesplazar + tFusionar

    def parte(x: Double): String = f1(100.0 * x / total)
    println("## Tabla 7: etapas de la solución con índice, secuencial, n = 400 (ms)")
    println()
    println("| Etapa | Tiempo (ms) | Parte del total (%) | Cómo se reparte |")
    println("|---|---|---|---|")
    println(s"| huellas | ${f1(tHuellas)} | ${parte(tHuellas)} | 2 tareas |")
    println(s"| indiceInvertido | ${f1(tIndice)} | ${parte(tIndice)} | 2 tareas |")
    println(s"| parejasCandidatas | ${f1(tCandidatas)} | ${parte(tCandidatas)} | no se reparte |")
    println(s"| similitudes (sobre las candidatas) | ${f1(tSimilitudes)} | " +
      s"${parte(tSimilitudes)} | hasta el corte |")
    println(s"| ranking | ${f1(tRanking)} | ${parte(tRanking)} | no se reparte |")
    println(s"| Suma de las etapas (secuencial) | ${f1(total)} | 100.0 | |")
    println(s"| desplazar + fusionar (solo en la paralela) | ${f1(extra)} | " +
      s"${parte(extra)} | no se reparte |")
    println()

    // Modelo de Amdahl con P núcleos, con lo medido arriba:
    //   T(P) = huellas / min(P, 2) + indice / min(P, 2) + extra + candidatas
    //          + ranking + similitudes / P
    val dos = math.min(nucleos, 2)
    val tModelo = tHuellas / dos + tIndice / dos + extra + tCandidatas +
      tRanking + tSimilitudes / nucleos
    val tTecho = tHuellas / 2 + tIndice / 2 + extra + tCandidatas + tRanking
    val noRepartido = extra + tCandidatas + tRanking
    val medidoS = medir(detectarConIndice(c400, k, umbral, p))
    val medidoP = medir(detectarConIndicePar(c400, k, umbral, p, corte))
    println("## Ley de Amdahl para la solución con índice, n = 400")
    println()
    println(s"Fracción que no se reparte f = (desplazar + fusionar + " +
      s"parejasCandidatas + ranking) / total = ${f1(100.0 * noRepartido / total)} %.")
    println(s"Techo clásico, 1 / f = ${f1(total / noRepartido)} veces.")
    println(s"Modelo con las etapas que solo parten en 2 tareas, con $nucleos núcleos: " +
      s"${f1(total / tModelo)} veces. Con infinitos núcleos: ${f1(total / tTecho)} veces.")
    println(s"Medido: secuencial ${f1(medidoS)} ms, paralela ${f1(medidoP)} ms, " +
      s"${f1(medidoS / medidoP)} veces.")
    println()
  }
}
