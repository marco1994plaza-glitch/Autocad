# ETARB – Etiquetas de numeración para árboles (AutoCAD / Civil 3D)

Carga con `APPLOAD` (o arrastra `ETARB.lsp` al dibujo).

| Comando | Función |
|---|---|
| `ETARB` | Selecciona árboles, elige el recorrido y crea las etiquetas |
| `ETARBROT` | Gira **todas** las etiquetas a la vez |
| `ETARBBORRAR` | Borra todas las etiquetas |

## Cómo funciona
- **Tipo de árbol**: bloque → nombre del bloque; punto Civil 3D → nombre del estilo de punto; `POINT` → capa.
- **Numeración**: independiente por tipo (1…n), según el orden de avance a lo largo del recorrido.
  - Con eje (polilínea, línea, spline…): se ordena por la progresiva de la proyección de cada árbol sobre el eje.
  - Sin eje: cadena de vecino más cercano desde un punto de inicio.
- **Etiqueta**: un bloque `ETQ_ARB` (punto en el árbol, guía fina, doble círculo y solo el número en Century Gothic) en la capa `Etiquetas`.
- **Giro en bloque**: cada etiqueta es una sola referencia de bloque que gira alrededor del árbol. `ETARBROT` acepta un ángulo, dos puntos u opción `Vista` (usa el giro de la vista del viewport; ejecútalo con el viewport activo, en MSPACE).
- La altura del número se pide al crear (por defecto 1.5 u). Ajústala a la escala del plano.
