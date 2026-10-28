# Informes

Aquí van los cuatro informes que pide el enunciado, en Markdown:

- `Proceso.md`: cómo se ejecuta cada función recursiva, paso a paso. Para este
  proyecto, la pila de llamados de `fragmentos` sobre un documento corto, y en
  qué operaciones se traduce cada expresión `for` que haya usado.
- `Correccion.md`: la argumentación de que cada función hace lo que el enunciado
  pide, más los casos de prueba. Dos propiedades se piden en concreto: que la
  huella de un documento de `t` palabras no supera los `t - k + 1` fragmentos, y
  que una pareja que no es candidata tiene similitud cero, de donde sale que las
  dos soluciones producen el mismo ranking.
- `Paralelizacion.md`: qué se paralelizó y qué ganancia dio, con las dos tablas
  del enunciado y la lectura de los resultados según la ley de Amdahl.
- `Conclusiones.md`: lo que queda del proyecto.

`Proceso.md` y `Correccion.md` ya existen y traen un ejemplo de lo que se
espera: el primero sigue la traza de `factorial(5)` con recursión de cola y
dibuja la pila con `mermaid`; el segundo argumenta por inducción estructural la
corrección del factorial y del máximo de una lista, y con estado, invariante y
transformación la de sus versiones iterativas. El informe del proyecto reemplaza
ese contenido y sigue la misma forma sobre las funciones del enunciado.

La inducción del ejemplo se traslada a este proyecto por el vector que recorre
`fragmentos`: el caso base es un vector más corto que `k` y el paso de inducción
añade una palabra al frente. Para el índice invertido el argumento es otro, y
conviene decirlo así: lo que se demuestra es que el resultado no depende del
orden en que se recorran las huellas, porque la unión de conjuntos es asociativa
y conmutativa. Ese mismo argumento es el que justifica que la versión paralela
dé lo mismo que la secuencial.

La notación matemática se escribe en LaTeX dentro del Markdown y los diagramas
se generan con `mermaid`. No se aceptan imágenes insertadas ni archivos por
fuera de esta carpeta.

El nombre completo y el código de cada integrante van en el `README.md` de la
raíz, que es la fuente de identificación del equipo.
