# Graph-Geometry

Tools for drawing finite graphs and putting geometric structure on them. A graph is given an atlas: a cover by subgraphs ("charts") that act as local coordinate patches, glued together the way charts form a manifold. This structure gives a discrete analogue of de Rham cohomology.

This repository accompanies my thesis on discrete geometry:

> Utkarsh Bajaj, *Discrete geometry and de-Rham cohomology of graphs*, [arXiv:2610.25302](https://arxiv.org/abs/2610.25302) (2026).

## Contents

| File | What it is |
| --- | --- |
| `GraphMaker.html` | Standalone web app for drawing graphs and charts in the browser |
| `GraphMaker.wl` | Interactive Mathematica app with the same idea, in 2D and 3D |

## GraphMaker (web app)

Open `GraphMaker.html` in any modern browser. It is a single self-contained file with no dependencies or install step.

- **Modes:** Select / Move, Add Vertex, Add Edge, Build Chart
- **Charts:** select vertices and edges, then click *Finish Chart* to add a translucent chart to the cover. Overlapping charts can each have their own colour.
- **Editing:** undo (Ctrl/Cmd+Z), and Delete/Backspace to remove the selected item
- **Mathematica interop:** paste a `Graph[...]` expression to load it, or copy the drawing back out as `Graph[...]` along with its cover as a list of vertex sets
- **JSON:** export and import drawings to save your work

## GraphMaker (Mathematica)

1. Open `GraphMaker.wl` in Mathematica.
2. Evaluate the cell (Shift+Enter). The `GraphMaker[]` panel appears below it.

Features:

- Build graphs in **2D or 3D**. In 3D you drag to rotate and click to act on whatever is under the cursor.
- Add, move, recolour, label and delete vertices and edges. Directed edges are supported.
- Build, edit and delete **charts**. They are drawn as rounded tubes around the chosen edges and vertices.
- **Import** any expression that evaluates to a `Graph` (e.g. `PetersenGraph[]`). Coordinates, styles, labels and dimension are read from it.
- **Export** to global variables:
  - `LastGraph`: a `Graph[...]` with coordinates and styling
  - `LastCover`: the list of vertex sets making up the cover
  - `LastCanvasGraphics`: the full rendered picture

The header comment of `GraphMaker.wl` documents every mode in detail.

## Citation

If you use this in your work, please cite the thesis:

```bibtex
@misc{bajaj2026discrete,
  author        = {Bajaj, Utkarsh},
  title         = {Discrete geometry and de-Rham cohomology of graphs},
  year          = {2026},
  eprint        = {2610.25302},
  archivePrefix = {arXiv},
  primaryClass  = {math.DG}
}
```
