# ETARB – Etiquetas de numeración para árboles (AutoCAD / Civil 3D)

Carga con `APPLOAD` (o arrastra `ETARB.lsp` al dibujo).

| Comando | Función |
|---|---|
| `ETARB` | 1) selecciona el **área**, 2) elige el **tipo** de punto, 3) recorrido, estilo, altura y giro; crea las etiquetas |
| `ETARBEDIT` | Edita **escala y giro** de todas las etiquetas y las reubica sin cruces |
| `ETARBESC` | Solo escala: altura del número o `Factor` (2 = doble, 0.5 = mitad); reubica sin cruces |
| `ETARBROT` | Solo giro: ángulo, dos puntos, `Vista` o `Incremento` |
| `ETARBESTILO` | Seleccionas un **área** y un **estilo**: cambia solo las etiquetas de esa área (conserva número, tipo, posición, giro y altura) |
| `ETARBACOMODAR` | Recalcula la posición de las etiquetas del área (Enter = todas) para evitar cruces |
| `ETARBFRENTE` | Trae todas las etiquetas al frente |
| `ETARBBORRAR` | Borra todas las etiquetas |

## Estilos
Todos llevan una marca rellena en el árbol y una aguja ahusada (fina en el árbol, más gruesa hacia la etiqueta).
1. **Medallon**: anillo grueso con filete fino interior.
2. **Llamada**: aguja y línea base ahusada, con el número encima.
3. **Hexagono**: doble hexágono, grueso y fino.

## Cómo funciona
- **Sin cruces**: cada estilo existe en 8 direcciones (bloques `ETQ_<ESTILO>_<ángulo>`). Al crear, cada etiqueta usa la primera dirección que no choque con otras etiquetas ni con otros árboles; si todas chocan, la de mayor holgura.
- **Fondo y frente**: cada bloque lleva un `WIPEOUT` que oculta lo que hay debajo, y las etiquetas se envían al frente (`DRAWORDER`). Si tu CAD no tiene `WIPEOUT`, se crean sin fondo.
- **Numeración**: si el tipo ya tiene etiquetas, `ETARB` pregunta `Continuar` (sigue desde el último número y salta los árboles ya etiquetados) o `Reiniciar` (borra las de ese tipo y empieza en 1).
- **Capa bloqueada**: los comandos desbloquean `Etiquetas` mientras trabajan y restauran el bloqueo al terminar.
- **Tipo de árbol**: bloque → nombre del bloque; punto Civil 3D → nombre del estilo de punto; `POINT` → capa.
- **Numeración**: independiente por tipo (1…n), según el orden de avance a lo largo del recorrido.
  - Con eje (polilínea, línea, spline…): por la progresiva de la proyección de cada árbol sobre el eje.
  - Sin eje: vecino más cercano desde un punto de inicio.
- **Reemplazo**: cada etiqueta guarda su tipo; al volver a etiquetar un tipo se reemplazan solo las etiquetas de ese tipo (las demás se conservan).
- **Capa** `Etiquetas`; fuente Century Gothic (estilo de texto `ETIQ_ARB`).
- **Escala y giro**: cada etiqueta es una sola referencia de bloque; escala y giro actúan alrededor del árbol, así guía y número se mueven juntos. `Vista` toma el giro del viewport activo (ejecútalo en MSPACE dentro del viewport).
