# Laboratorio 1 - Paradigmas de Programación (FaMAF)

Este proyecto corresponde al primer laboratorio de la materia *Paradigmas de Programación* (2025), en el que trabajamos con el lenguaje Haskell y la biblioteca **Gloss** para construir una DSL (Domain Specific Language) que describe figuras geométricas y permite interpretarlas visualmente.

## Enfoque del trabajo

El laboratorio nos llevó a diseñar un pequeño lenguaje funcional que describe transformaciones sobre dibujos: rotaciones, simetrías, apilamientos, superposiciones, etc. Definimos este lenguaje utilizando tipos algebraicos, y luego lo interpretamos gráficamente con **Gloss**.

La estructura del trabajo fue organizada en varios módulos:
- `Dibujo.hs`: definimos el tipo `Dibujo a` y las operaciones para construir dibujos complejos a partir de primitivas.
- `Interp.hs`: (no incluido arriba pero presumiblemente usado) se encargó de interpretar los dibujos en imágenes de Gloss.
- `Main.hs`: se utilizó para correr una visualización simple con grilla de referencia.

## Qué hicimos

Fuimos desarrollando el laboratorio en fases:
1. **Definición del DSL**: creamos `Dibujo.hs` con los constructores `Basica`, `Rotar`, `Espejar`, `Apilar`, `Juntar`, `Encimar`, y combinadores como `cuarteto`, `encimar4`, `ciclar`, etc.
2. **Pruebas visuales**: usando `Main.hs`, generamos visualizaciones para testear nuestras composiciones.
3. **Interpretación gráfica**: trabajamos sobre el módulo de interpretación usando Gloss, generando imágenes a partir de nuestro DSL.

## Archivos extra

Durante el desarrollo se nos ocurrieron dos ideas que excedían los requerimientos del laboratorio, y las implementamos como archivos extra en una carpeta aparte:
se ejecutan haciendo 'cabal run particulas' y 'cabal run pescados' respectivamente.

### `extra/Particulas.hs`

Creamos una simulación interactiva utilizando la función `play` de Gloss. En este archivo:

- Se representa un conjunto de partículas en pantalla.
- Detectamos la posición actual del mouse.
- Las partículas reaccionan alejándose del cursor, generando una sensación de "repulsión".
- Nos sirvió como experimento para entender mejor el modelo de eventos y estado de Gloss.

Esta parte fue especialmente divertida porque combinamos conceptos funcionales con interactividad en tiempo real.

### `extra/Pescados.hs`

Inspirados por el ejemplo `Escher`, desarrollamos una versión más detallada y personalizada del dibujo:

- Modificamos el estilo visual para lograr un efecto más elaborado y simétrico.
- Experimentamos con combinaciones más complejas de rotaciones y superposiciones.
- Esta parte nos ayudó a explorar más a fondo el poder expresivo de la DSL.

## Reflexiones

Este laboratorio nos dio una gran introducción a:
- El diseño de lenguajes embebidos (DSL).
- El uso de tipos algebraicos en Haskell.
- La separación entre construcción y visualización de estructuras.
- La biblioteca Gloss para gráficos en Haskell.

Además, el hecho de que pudimos extender el proyecto con ideas propias nos motivó y nos ayudó a apropiarnos del contenido. Sentimos que terminamos con un entendimiento más profundo no solo del material teórico, sino también de cómo aplicarlo de forma creativa.

---

**Autores**: Clemente Ivetta, Ignacio Hernandez, Nehuen Guevara, Andres Villagra  
**Año**: 2025
