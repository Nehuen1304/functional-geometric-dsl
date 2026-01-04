# Functional Geometric DSL Engine λ

[![Haskell](https://img.shields.io/badge/Language-Haskell-5e5086?style=flat&logo=haskell&logoColor=white)](https://www.haskell.org/)
[![Library](https://img.shields.io/badge/Graphics-Gloss-green?style=flat)](https://hackage.haskell.org/package/gloss)
[![Paradigm](https://img.shields.io/badge/Paradigm-Functional-blue?style=flat)](https://en.wikipedia.org/wiki/Functional_programming)

**A declarative Domain Specific Language (DSL)** designed for modeling complex geometric structures through function composition. This project leverages **Algebraic Data Types (ADTs)** to create an expressive, type-safe system for graphic generation, rendering directly via OpenGL/Gloss.

---

## 🧠 Core Engineering Features

### 1. Compositional Algebra (The DSL)
The core architecture is built around a recursive Abstract Syntax Tree (AST) that separates **Syntax** (description) from **Semantics** (rendering).

```haskell
-- The Core Algebra Definition
data Dibujo a = 
      Figura a                 -- Atomic Primitive
    | Rotar (Dibujo a)         -- Geometric Transformation
    | Espejar (Dibujo a)       -- Reflection
    | Rot45 (Dibujo a)         -- Angular Composition
    | Apilar Float Float (Dibujo a) (Dibujo a) -- Vertical Stack
    | Juntar Float Float (Dibujo a) (Dibujo a) -- Horizontal Stack
    | Encimar (Dibujo a) (Dibujo a)            -- Z-index Overlay
```

### 2. Recursive Tessellation (Escher Project)
Implemented complex recursive combinators to generate non-trivial patterns similar to M.C. Escher's artwork.
* **Logic:** Uses higher-order functions (`cycle`, `quartet`) to manipulate the geometry grid.
* **Implementation:** Located in `extra/Pescados.hs`.

### 3. Interactive Physics Simulation
Beyond static rendering, the engine supports real-time event handling.
* **Particle System:** A simulation of particles reacting to mouse input (repulsion forces).
* **State Management:** Pure functional state handling via Gloss `play` function.
* **Implementation:** Located in `extra/Particulas.hs`.

---

## 🎨 Design Pattern: Interpreter Pattern

The system implements the **Interpreter Pattern** to provide multiple views for the same data structure:
1.  **Graphical Interpreter:** Renders the AST to a window using OpenGL (`Interp.hs`).
2.  **String Interpreter:** (Debug) Serializes the tree structure for inspection.
3.  **Logical Fold:** Computes metadata (depth, node count) without rendering.

---

## 🚀 Usage

### Running the Tessellation Demo
```bash
cabal run pescados
```

### Running the Particle Simulation
```bash
cabal run particulas
```

### Main Visualization
```bash
cabal run
```

---

### 📝 Credits
Developed as a Functional Programming Capstone for **CS Paradigms** at **FaMAF - UNC**.
* **Team:** Nehuen Guevara, Clemente Ivetta, Ignacio Hernandez, Andres Villagra.