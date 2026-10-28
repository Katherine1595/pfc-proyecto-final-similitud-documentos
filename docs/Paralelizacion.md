# Informe de paralelización

Reemplace el contenido de este archivo por el informe de su equipo. La forma de
las tablas y las preguntas que hay que responder son las del enunciado.

## Qué se paralelizó

Explique, función por función, qué se reparte y cómo. Diga dónde hay
paralelismo de datos, donde cada documento se resume por separado, y dónde hay
paralelismo de tareas, donde se parte en dos el trabajo y se vuelve a partir
hasta el corte.

Diga también qué decidió no paralelizar y por qué.

## Condiciones de la medición

| Parámetro | Valor |
|---|---|
| Palabras por documento, `t` | |
| Vocabulario, `v` | |
| Tamaño de fragmento, `k` | |
| Corte | |
| Núcleos de la máquina | |
| Versión de la JVM | |

Diga cuántas veces midió cada punto y si reporta la mediana o el promedio. Una
sola corrida no sirve: la primera paga la compilación de la JVM.

## Tabla 1: la solución directa contra su versión paralela

| Docs `n` | Secuencial (ms) | Paralela (ms) | Aceleración (%) |
|---|---|---|---|
| 50 | | | |
| 100 | | | |
| 200 | | | |
| 400 | | | |

## Tabla 2: los dos algoritmos secuenciales, y cuántas parejas sobreviven

| Docs `n` | Parejas totales | Candidatas | Directo (ms) | Con índice (ms) |
|---|---|---|---|---|
| 50 | | | | |
| 100 | | | | |
| 200 | | | | |
| 400 | | | | |

## Tabla 3: la solución con índice, secuencial contra paralela

Esta es la que falta en el enunciado y la que suele dar la sorpresa. Mídala.

| Docs `n` | Con índice (ms) | Con índice paralela (ms) | Aceleración (%) |
|---|---|---|---|
| 50 | | | |
| 100 | | | |
| 200 | | | |
| 400 | | | |

## Análisis

Responda cada punto con los números de sus tablas a la vista:

1. En qué tamaños de `(n, t)` el paralelismo gana, y en qué tamaños la
   sobrecarga de crear tareas pesa más que el trabajo repartido.
2. Qué valor de corte funciona mejor en su máquina, y por qué ese.
3. Qué efecto tienen el vocabulario `v` y el tamaño de fragmento `k` sobre el
   número de parejas candidatas, con la fracción de supervivencia medida.
4. Cuál de los dos cambios rinde más, el índice invertido o el paralelismo.
5. Si las dos ganancias se acumulan. Aplique el índice y el paralelismo a la vez
   y compare contra aplicar solo el índice. Si la aceleración que traía el
   paralelismo se desvanece, explique a dónde se fue el trabajo: qué parte del
   cálculo domina el tiempo cuando el índice ya descartó la mayoría de las
   parejas, y por qué esa parte no se está repartiendo.
6. Cuál es el techo de aceleración de su implementación según la ley de Amdahl,
   y cómo se compara con lo que midió.

La fracción secuencial que entra en la ley de Amdahl no se supone: se mide.
Diga de dónde sacó la suya.
