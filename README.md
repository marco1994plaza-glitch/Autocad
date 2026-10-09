# ETARB – Etiquetas de numeración para árboles (AutoCAD / Civil 3D)

Carga con `APPLOAD` (o arrastra `ETARB.lsp` al dibujo).

| Comando | Función |
|---|---|
| `ETARB` | Selecciona árboles, elige recorrido, estilo, altura y giro, y crea las etiquetas |
| `ETARBEDIT` | Edita **escala y giro** de todas las etiquetas de una vez |
| `ETARBESC` | Solo escala: altura del número o `Factor` (2 = doble, 0.5 = mitad) |
| `ETARBROT` | Solo giro: ángulo, dos puntos, `Vista` o `Incremento` |
| `ETARBESTILO` | Cambia el estilo de todas las etiquetas existentes (conserva número, posición, giro y altura) |
| `ETARBBORRAR` | Borra todas las etiquetas |

## Estilos
1. **Clasico**: guía a 45° + doble círculo.
2. **Llamada**: guía + número sobre una línea base horizontal.
3. **Rombo**: guía + doble rombo.

## Cómo funciona
- **Tipo de árbol**: bloque → nombre del bloque; punto Civil 3D → nombre del estilo de punto; `POINT` → capa.
- **Numeración**: independiente por tipo (1…n), según el orden de avance a lo largo del recorrido.
  - Con eje (polilínea, línea, spline…): por la progresiva de la proyección de cada árbol sobre el eje.
  - Sin eje: vecino más cercano desde un punto de inicio.
- **Capa** `Etiquetas`; fuente Century Gothic (estilo de texto `ETIQ_ARB`).
- **Escala y giro**: cada etiqueta es una sola referencia de bloque; escala y giro actúan alrededor del árbol, así guía y número se mueven juntos. `Vista` toma el giro del viewport activo (ejecútalo en MSPACE dentro del viewport).
