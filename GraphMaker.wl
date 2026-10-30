(* ::Package:: *)

(*
  GraphMaker.wl
  -------------
  An interactive Mathematica app for manually building graphs (2D or 3D) by
  clicking, drawing translucent "charts" around subsets of vertices to
  indicate a cover by subgraphs, and importing existing Graph[] objects for
  further editing.

  HOW TO RUN:
    1. Open this file in Mathematica (double-click it, or File > Open).
    2. Select the whole cell and evaluate it (Shift+Enter), or Ctrl+A then Shift+Enter.
    3. The app panel will appear as output below the code.

  GRAPH CONTROLS:
    - Mode "Add Vertex"     : click empty canvas to place a new vertex. In
                               3D, the click sets x,y; use the "Height (Z)"
                               slider to set the z-coordinate before clicking.
    - Mode "Add Edge"       : click one vertex, then click a second vertex to
                               connect them. Toggle "Directed" first if you
                               want an arrow.
    - Mode "Move"           : click and drag a vertex to reposition it (in 3D
                               it slides within its own z-plane).
    - Mode "Set Height (3D)": click a vertex, then drag the "Height (Z)"
                               slider to change its z-coordinate live.
    - Mode "Recolor Vertex" : click a vertex to paint it with "Vertex color".
    - Mode "Recolor Edge"   : click near an edge to paint it with "Edge color".
    - Mode "Label Vertex"   : type text into "Vertex label", then click a
                               vertex to give it that label (leave the field
                               blank and click to reset to the default ID).
    - Mode "Delete Vertex"  : click a vertex to remove it (and its edges, and
                               removes it from any charts it belongs to).
    - Mode "Delete Edge"    : click near an edge to remove it.

  DIMENSION:
    - "Dimension: 2D/3D" switches between a flat canvas and a 3D workspace.
      There is only ever ONE canvas: a flat Graphics in 2D, and the rotatable
      3D scene in 3D. Every mode above works in whichever canvas is showing.
    - In the 3D canvas, DRAG rotates the scene and a CLICK (press and release
      without moving) acts on the vertex/edge/chart under the cursor. Picking
      casts the mouse ray through the 3D box and takes the thing nearest the
      camera, so you select what you can actually see. Vertices win over edges.
    - "Add Vertex" in 3D places the new vertex where the mouse ray crosses the
      horizontal plane at the current "Height (Z)", so the slider chooses the
      layer you are drawing into. "Move" drags a vertex within its own
      z-plane (rotation is suspended while Move is the active mode; switch
      back to another mode to rotate again), and "Set Height (3D)" clicks a
      vertex and then follows the slider.
    - Chart membership shows up in 3D as a colored halo on a vertex and a
      colored tube along an edge; the vertex picked up by "Add Edge" or
      "Set Height" gets a red halo.

  COVER (CHART) CONTROLS:
    - Mode "Build Chart"    : click vertices and/or edges one by one to
                               toggle them into the chart you're building
                               (vertices get a colored ring, edges get a
                               colored highlight). Edge membership is chosen
                               independently of vertex membership. Pick
                               "Chart color"/"Chart opacity"/"Chart radius"
                               (how fat the tube around the chart is), then
                               press "Finish Chart" to add it to the cover,
                               or "Cancel Chart" to discard the selection.
    - Mode "Edit Chart"     : click a chart (or use its "Edit" button) to
                               select it, then click vertices/edges to
                               toggle their membership in that chart. The
                               color/opacity/radius controls now act on the
                               chart being edited, live.
    - Mode "Delete Chart"   : click inside a chart to remove it.
    - Charts are drawn as translucent, rounded regions (or rounded solids in
      3D) behind the graph, so you can still see the vertices/edges through
      them. You can add several overlapping charts with different colors.
      A chart's shape is a union of capsules around its selected edges plus
      circles around any of its selected vertices not touched by a selected
      edge - it does NOT automatically include every induced edge.

  IMPORT:
    - Type the name of a variable or expression that evaluates to a Graph[]
      object (e.g. LastGraph, or PetersenGraph[]) into the "Import" field
      and press "Load". Vertex positions, colors, edge directions/colors,
      custom VertexLabels (if the graph's VertexLabels option gives a plain
      string per vertex), and 2D vs 3D are all read from the graph.
    - Graph3D[...] works too: it is an ordinary Graph with a 3D embedding, so
      loading one switches the app into 3D automatically and the rotatable 3D
      scene becomes the editing canvas.

  EXPORT:
    - "Include vertex labels in exported image" toggles whether the vertex
      labels appear in the exported picture (turn off for a clean image).
    - "Export as Graph[...]"    : builds a real Graph[] object (with
                                   VertexCoordinates/VertexStyle/EdgeStyle/
                                   VertexLabels matching your drawing, in 2D
                                   or 3D), stores it in the global variable
                                   Global`LastGraph, and prints it inline so
                                   you can inspect and copy it directly.
    - "Export Cover + Picture"  : stores the list of vertex sets making up
                                   the cover in Global`LastCover, stores the
                                   full rendered picture (graph + charts) as a
                                   Graphics[]/Graphics3D[] object in
                                   Global`LastCanvasGraphics, and prints both.
*)

nearestVertex[pos_, verts_Association, thresh_: 0.35] := Module[{ds},
  If[Length[verts] == 0, Return[None]];
  ds = KeyValueMap[{#1, EuclideanDistance[#2, pos]} &, verts];
  ds = Select[ds, #[[2]] < thresh &];
  If[Length[ds] == 0, None, First[SortBy[ds, Last]][[1]]]
];

(* Distance from point p to the segment a-b. Dimension-agnostic: used both for
   2D hit-testing on the flat canvas and for 3D ray picking. *)
distToSegment[p_, a_, b_] := Module[{ab, t, proj},
  ab = b - a;
  If[ab . ab == 0, Return[EuclideanDistance[p, a]]];
  t = Clip[((p - a) . ab)/(ab . ab), {0, 1}];
  proj = a + t*ab;
  EuclideanDistance[p, proj]
];

(* Smallest distance between the edge segment a-b and the click ray r1-r2,
   found by sampling points along the edge. Cheap and robust for the graph
   sizes this app deals with. *)
edgeRayDistance[a_, b_, r1_, r2_, n_: 24] :=
  Min[Table[distToSegment[a + t*(b - a), r1, r2], {t, 0., 1., 1./n}]];

(* 3D picking. Given the ray {r1, r2} under the mouse (from
   MousePosition["Graphics3DBoxIntercepts"]), returns {"Vertex", id} or
   {"Edge", edgeAssoc} for whatever the ray passes closest to, or None.
   Vertices win over edges, and among candidates the one nearest the camera
   end r1 wins, so you pick the thing you can actually see. *)
pickFromRay[verts_Association, edgeList_List, {r1_, r2_}, vThresh_: 0.5, eThresh_: 0.35] :=
 Module[{vCand, eCand},
  vCand = Select[Normal[verts], distToSegment[Last[#], r1, r2] < vThresh &];
  If[vCand =!= {},
   Return[{"Vertex", First[SortBy[vCand, EuclideanDistance[Last[#], r1] &]][[1]]}]];
  eCand = Select[edgeList,
    edgeRayDistance[verts[#["From"]], verts[#["To"]], r1, r2] < eThresh &];
  If[eCand =!= {},
   Return[{"Edge", First[SortBy[eCand,
      EuclideanDistance[(verts[#["From"]] + verts[#["To"]])/2, r1] &]]}]];
  None
];

(* Where the click ray {r1, r2} crosses the horizontal plane at height z.
   This is what turns a 2D mouse position into a definite 3D point when
   placing or dragging a vertex: the x,y come from the mouse, the z from the
   "Height (Z)" control. If the ray is (nearly) parallel to the plane there
   is no crossing, so fall back to the middle of the ray. *)
rayPlaneZ[{r1_, r2_}, z_] := Module[{d = r2 - r1},
  If[Abs[d[[3]]] < 10.^-9,
   (r1 + r2)/2,
   r1 + ((z - r1[[3]])/d[[3]])*d]
];

(* True if the click ray passes through the body of a chart, i.e. within pad
   of one of the chart's selected edges or selected vertices. This is the 3D
   counterpart of RegionMember on the flat canvas, used to click a whole
   chart to select or delete it. It works off the chart's definition rather
   than its rendered shape, so it stays exact and instant no matter how the
   shape itself ended up being built. *)
chartHitsRay[verts_Association, edgeList_List, chart_Association, pad_, {r1_, r2_}] :=
  AnyTrue[Select[edgeList, MemberQ[chart["Edges"], #["Id"]] &],
    edgeRayDistance[verts[#["From"]], verts[#["To"]], r1, r2] < pad &] ||
  AnyTrue[chart["Vertices"], distToSegment[verts[#], r1, r2] < pad &];

(* Drops to {x,y} for hit-testing/rendering on the flat editing canvas when
   the graph is in 3D mode; identity in 2D mode. *)
projectVertices[d_, v_Association] := If[d === 3, Most /@ v, v];

(* Recenters and uniformly rescales a list of embedding coordinates (as
   produced by GraphEmbedding) so they fill the canvas nicely, regardless of
   how small/large or off-center the original layout was. Keeps aspect ratio
   (2D or 3D) by scaling all coordinates by the same factor. *)
rescaleEmbedding[emb_List, target_: 8.] := Module[{center, shifted, maxAbs},
  If[Length[emb] == 0, Return[emb]];
  center = Mean[emb];
  shifted = # - center & /@ emb;
  maxAbs = Max[Abs[Flatten[shifted]], 10^-6];
  (target/maxAbs)*shifted
];

(* Draws an edge from a to b. Undirected edges are a plain Line. Directed
   edges are drawn as a full Line (so the edge is visible its whole length)
   plus a short Arrow centered on the midpoint, so the arrowhead reads as
   pointing along the middle of the edge rather than sitting at the target
   vertex. Works for both 2D and 3D points. *)
edgePrimitive[a_, b_, directedQ_, frac_: 0.3, arrowSize_: 0.03] := Module[{mid, halfVec},
  If[!directedQ || a == b, Line[{a, b}],
   mid = (a + b)/2;
   halfVec = (b - a)*frac/2;
   {Line[{a, b}], Arrowheads[arrowSize], Arrow[{mid - halfVec, mid + halfVec}]}]
];

(* Builds a smooth, rounded polygon enclosing a set of 2D points, by
   inflating each point into a small circle and taking the convex hull
   of all the sampled circle points. Works for 1, 2, or many points. *)
chartPolygon[pts_List, pad_: 0.55, n_: 24] := Module[{circPts, chm},
  circPts = Flatten[
    Table[# + pad*{Cos[t], Sin[t]}, {t, 0., 2 Pi - 2 Pi/n, 2 Pi/n}] & /@ pts, 1];
  chm = ConvexHullMesh[circPts];
  First[Normal[MeshPrimitives[chm, 2]]]
];

(* 3D analogue of chartPolygon: inflates each point into a small sphere of
   sample points and takes the convex hull, giving a smooth rounded solid. *)
chartPolyhedron3D[pts_List, pad_: 0.55, n_: 8] := Module[{sphPts},
  sphPts = Flatten[
    Table[# + pad*{Sin[u] Cos[v], Sin[u] Sin[v], Cos[u]},
      {u, 0., Pi, Pi/n}, {v, 0., 2 Pi - 2 Pi/n, 2 Pi/n}] & /@ pts, 2];
  ConvexHullMesh[sphPts]
];

(* Builds a "tube" cover shape for a chart: instead of the convex hull of all
   the chart's vertices (which fills in the interior of the shape), this
   unions a rounded capsule (chartPolygon on the 2 endpoints) along each edge
   in edgeList whose Id is in edgeIds (the chart's explicitly chosen edges),
   plus a small disk for any vertex in ids not touched by a selected edge.
   Edge membership is chosen independently of vertex membership. The result
   traces the path/subgraph through the vertices rather than covering the
   area between them. *)
chartTube[ptsAssoc_Association, ids_List, edgeList_List, edgeIds_List, pad_: 0.55, n_: 24] :=
 Module[{selEdges, touched, isolated, pieces},
  selEdges = Select[edgeList, MemberQ[edgeIds, #["Id"]] &];
  touched = Union @@ (({#["From"], #["To"]}) & /@ selEdges);
  isolated = Complement[ids, touched];
  pieces = Join[
    chartPolygon[{ptsAssoc[#["From"]], ptsAssoc[#["To"]]}, pad, n] & /@ selEdges,
    chartPolygon[{ptsAssoc[#]}, pad, n] & /@ isolated];
  Which[pieces === {}, {}, Length[pieces] == 1, First[pieces], True, RegionUnion @@ pieces]
];

(* 3D analogue of chartTube, built from chartPolyhedron3D capsules/spheres.

   RegionUnion of overlapping capsule meshes is not numerically reliable: for
   some vertex layouts it fails to build a closed boundary surface (warning
   BoundaryMeshRegion::bsuncl) and quietly returns an unevaluated
   BooleanRegion instead. That is still RegionQ-true, but Graphics3D cannot
   draw it, which is why an affected chart used to vanish from the canvas.
   So the union is only used when it really produced a mesh; otherwise fall
   back to drawing the capsules and balls themselves, which always render.
   The fallback looks the same except for faint seams where two of them meet. *)
chartTube3D[ptsAssoc_Association, ids_List, edgeList_List, edgeIds_List, pad_: 0.55, n_: 8] :=
 Module[{selEdges, touched, isolated, segs, pieces, u},
  selEdges = Select[edgeList, MemberQ[edgeIds, #["Id"]] &];
  touched = Union @@ (({#["From"], #["To"]}) & /@ selEdges);
  isolated = Complement[ids, touched];
  segs = ({ptsAssoc[#["From"]], ptsAssoc[#["To"]]}) & /@ selEdges;
  pieces = Join[
    chartPolyhedron3D[#, pad, n] & /@ segs,
    chartPolyhedron3D[{ptsAssoc[#]}, pad, n] & /@ isolated];
  Which[
   pieces === {}, {},
   Length[pieces] == 1, First[pieces],
   True,
   u = Quiet[RegionUnion @@ pieces];
   If[BoundaryMeshRegionQ[u], u,
    Join[CapsuleShape[#, pad] & /@ segs, Ball[ptsAssoc[#], pad] & /@ isolated]]]
];

(* Each chart carries its own tube radius under "Radius" (set by the "Chart
   radius" slider). Charts made before that key existed get the old default. *)
chartRadius[b_Association] := Lookup[b, "Radius", 0.55];

GraphMaker[] := DynamicModule[
  {
    vertices = <||>, vertexColors = <||>, vertexLabels = <||>, edges = {},
    charts = {}, buildingChart = {}, buildingChartEdges = {}, editingChartIndex = None,
    nextId = 1, nextEdgeId = 1, mode = "AddVertex", selected = None, moving = None,
    vColor = Black, eColor = Black, directedQ = True,
    chartColor = LightBlue, chartOpacity = 0.35, chartPad = 0.55,
    dim = 2, newZ = 0., exportLabels = True, showCanvasLabels = True,
    importExpr = "", labelText = "",
    edgeAnimA = None, edgeAnimB = None, edgeAnimT = 0.,
    mouseDown3D = None,
    history = {}, pushUndo, undo, adjEditIx, animateEdge, toggleChartPick,
    dropVertex, dropEdgeId, handle3DClick
  },

  (* Undo support: pushUndo[] snapshots the editable state onto a stack
     (before a mutating action); undo[] (bound to Ctrl+Z / Cmd+Z) restores
     the most recent snapshot. *)
  pushUndo[] := (
    AppendTo[history, <|
      "vertices" -> vertices, "vertexColors" -> vertexColors,
      "vertexLabels" -> vertexLabels,
      "edges" -> edges, "charts" -> charts, "buildingChart" -> buildingChart,
      "buildingChartEdges" -> buildingChartEdges,
      "nextId" -> nextId, "nextEdgeId" -> nextEdgeId, "dim" -> dim|>];
    If[Length[history] > 50, history = Rest[history]]);

  undo[] := If[Length[history] > 0,
    Module[{s = Last[history]},
      vertices = s["vertices"]; vertexColors = s["vertexColors"];
      vertexLabels = s["vertexLabels"];
      edges = s["edges"]; charts = s["charts"]; buildingChart = s["buildingChart"];
      buildingChartEdges = s["buildingChartEdges"];
      nextId = s["nextId"]; nextEdgeId = s["nextEdgeId"]; dim = s["dim"];
      selected = None; moving = None; editingChartIndex = None; edgeAnimA = None;
      history = Most[history]]];

  (* Keeps editingChartIndex pointing at the same logical chart after the
     chart at position delIx is removed from the charts list (which shifts
     the indices of everything after it). *)
  adjEditIx[delIx_] := editingChartIndex = Which[
     editingChartIndex === None, None,
     editingChartIndex === delIx, None,
     editingChartIndex > delIx, editingChartIndex - 1,
     True, editingChartIndex];

  (* Removes a vertex together with everything that refers to it: its colour
     and label, its incident edges, and the (now dangling) vertex/edge ids
     inside every chart and inside the chart being built. Charts left with
     nothing in them are dropped. Shared by the 2D and 3D canvases. *)
  dropVertex[v_] := Module[{removedIds},
    removedIds = #["Id"] & /@ Select[edges, #["From"] === v || #["To"] === v &];
    vertices = KeyDrop[vertices, v];
    vertexColors = KeyDrop[vertexColors, v];
    vertexLabels = KeyDrop[vertexLabels, v];
    edges = Select[edges, #["From"] =!= v && #["To"] =!= v &];
    charts = Select[
      (ReplacePart[#, {Key["Vertices"] -> DeleteCases[#["Vertices"], v],
         Key["Edges"] -> DeleteCases[#["Edges"], Alternatives @@ removedIds]}]) & /@ charts,
      Length[#["Vertices"]] > 0 || Length[#["Edges"]] > 0 &];
    buildingChart = DeleteCases[buildingChart, v];
    buildingChartEdges = DeleteCases[buildingChartEdges, Alternatives @@ removedIds];
    If[selected === v, selected = None]];

  (* Removes the edge with the given Id and purges it from every chart. *)
  dropEdgeId[eid_] := (
    edges = Select[edges, #["Id"] =!= eid &];
    charts = (ReplacePart[#, Key["Edges"] -> DeleteCases[#["Edges"], eid]]) & /@ charts;
    buildingChartEdges = DeleteCases[buildingChartEdges, eid]);

  (* Brief animated "growing line" preview drawn from a to b when a new edge
     is added. Purely cosmetic: a handful of Pause[] frames, so it stays
     fast and doesn't noticeably slow the app down. *)
  animateEdge[a_, b_] := (
    edgeAnimA = a; edgeAnimB = b;
    Do[edgeAnimT = f; Pause[0.02], {f, 0.1, 1.0, 0.1}];
    edgeAnimA = None; edgeAnimB = None; edgeAnimT = 0.);

  (* Takes a pick from the 3D view ({"Vertex", id} or {"Edge", edgeAssoc}) and
     toggles it into/out of the chart currently being built or edited. This is
     the 3D counterpart of the click handling on the flat canvas. *)
  toggleChartPick[pick_] := If[pick =!= None,
    Module[{kind = First[pick], item = Last[pick]},
     Which[
      mode === "BuildChart" && kind === "Vertex",
      If[MemberQ[buildingChart, item],
       buildingChart = DeleteCases[buildingChart, item],
       AppendTo[buildingChart, item]],

      mode === "BuildChart" && kind === "Edge",
      Module[{eid = item["Id"]},
       If[MemberQ[buildingChartEdges, eid],
        buildingChartEdges = DeleteCases[buildingChartEdges, eid],
        AppendTo[buildingChartEdges, eid]]],

      mode === "EditChart" && editingChartIndex =!= None && kind === "Vertex",
      pushUndo[];
      charts[[editingChartIndex, "Vertices"]] =
       If[MemberQ[charts[[editingChartIndex, "Vertices"]], item],
        DeleteCases[charts[[editingChartIndex, "Vertices"]], item],
        Append[charts[[editingChartIndex, "Vertices"]], item]],

      mode === "EditChart" && editingChartIndex =!= None && kind === "Edge",
      pushUndo[];
      Module[{eid = item["Id"]},
       charts[[editingChartIndex, "Edges"]] =
        If[MemberQ[charts[[editingChartIndex, "Edges"]], eid],
         DeleteCases[charts[[editingChartIndex, "Edges"]], eid],
         Append[charts[[editingChartIndex, "Edges"]], eid]]]]]];

  (* Every editing mode, driven by a click in the 3D canvas. The mouse ray
     through the 3D box picks the vertex / edge / chart under the cursor;
     where a click has to name a point in space rather than an existing
     object (AddVertex), the point is taken where the ray crosses the
     horizontal plane at the current "Height (Z)". This mirrors the flat
     canvas mode for mode, so a 3D graph never needs the 2D view. "Move" is
     absent here because it is driven by dragging, not clicking. *)
  handle3DClick[ray_] := Module[{pick = pickFromRay[vertices, edges, ray], v, e, hitChart},
    v = If[pick =!= None && First[pick] === "Vertex", Last[pick], None];
    e = If[pick =!= None && First[pick] === "Edge", Last[pick], None];
    hitChart[] := SelectFirst[Reverse[Range[Length[charts]]],
      chartHitsRay[vertices, edges, charts[[#]], chartRadius[charts[[#]]], ray] &,
      None];
    Which[
     mode === "AddVertex",
     pushUndo[];
     vertices[nextId] = rayPlaneZ[ray, newZ];
     vertexColors[nextId] = vColor; vertexLabels[nextId] = nextId; nextId++,

     mode === "AddEdge",
     If[v =!= None,
      If[selected === None, selected = v,
       If[v =!= selected,
        pushUndo[];
        AppendTo[edges, <|"From" -> selected, "To" -> v,
          "Directed" -> directedQ, "Color" -> eColor, "Id" -> nextEdgeId|>];
        nextEdgeId++];
       selected = None]],

     mode === "SetHeight",
     If[v =!= None, pushUndo[]; selected = v; newZ = vertices[v][[3]]],

     mode === "RecolorVertex",
     If[v =!= None, pushUndo[]; vertexColors[v] = vColor],

     mode === "LabelVertex",
     If[v =!= None,
      pushUndo[];
      vertexLabels[v] = If[StringTrim[labelText] === "", v, labelText]],

     mode === "RecolorEdge",
     If[e =!= None,
      pushUndo[]; edges = ReplaceAll[edges, e -> Append[e, "Color" -> eColor]]],

     mode === "DeleteVertex",
     If[v =!= None, pushUndo[]; dropVertex[v]],

     mode === "DeleteEdge",
     If[e =!= None, pushUndo[]; dropEdgeId[e["Id"]]],

     mode === "BuildChart",
     toggleChartPick[pick],

     (* Clicking a vertex or edge toggles its membership in the chart being
        edited; clicking anywhere else picks up whichever chart body the ray
        passes through. *)
     mode === "EditChart",
     If[pick =!= None && editingChartIndex =!= None,
      toggleChartPick[pick],
      Module[{hit = hitChart[]},
       If[hit =!= None,
        editingChartIndex = hit;
        chartColor = charts[[hit, "Color"]];
        chartOpacity = charts[[hit, "Opacity"]];
        chartPad = chartRadius[charts[[hit]]]]]],

     mode === "DeleteChart",
     Module[{hit = hitChart[]},
      If[hit =!= None, pushUndo[]; adjEditIx[hit]; charts = Delete[charts, hit]]]
    ]];

  Panel[Column[{
    Row[{Style["Dimension: ", Bold],
      RadioButtonBar[
       Dynamic[dim, Function[nd,
         vertices = Association[KeyValueMap[
            #1 -> Which[
               nd == 3 && dim == 2, Append[#2, 0.],
               nd == 2 && dim == 3, Most[#2],
               True, #2] &, vertices]];
         dim = nd]],
       {2 -> "2D", 3 -> "3D"}]}],
    Dynamic[
      RadioButtonBar[Dynamic[mode], Join[{
          "AddVertex" -> "Add Vertex", "AddEdge" -> "Add Edge", "Move" -> "Move",
          "RecolorVertex" -> "Recolor Vertex", "RecolorEdge" -> "Recolor Edge",
          "LabelVertex" -> "Label Vertex",
          "DeleteVertex" -> "Delete Vertex", "DeleteEdge" -> "Delete Edge"},
         If[dim == 3, {"SetHeight" -> "Set Height (3D)"}, {}]]],
      TrackedSymbols :> {dim}],
    Row[{Style["Current mode: ", Bold], Dynamic[mode]}],
    Row[{"Vertex label: ", InputField[Dynamic[labelText], String, ImageSize -> 150],
      "   (Label Vertex mode: click a vertex to apply; blank = reset to ID)"}],
    Row[{Checkbox[Dynamic[showCanvasLabels]], " Show vertex labels on canvas"}],
    Dynamic[If[dim == 3,
      Row[{Style["Height (Z): ", Bold],
        Slider[Dynamic[newZ, Function[z,
           newZ = z;
           If[mode === "SetHeight" && selected =!= None,
             vertices[selected] = ReplacePart[vertices[selected], 3 -> z]]]],
         {-6, 6}], "  z = ", Dynamic[NumberForm[N[newZ], 2]]}],
      ""]],
    Row[{Style["Cover: ", Bold],
      RadioButtonBar[Dynamic[mode], {
        "BuildChart" -> "Build Chart (click vertices/edges)",
        "EditChart" -> "Edit Chart (click chart, then vertices/edges)",
        "DeleteChart" -> "Delete Chart"}]}],
    Row[{
      Checkbox[Dynamic[directedQ]], " Directed   ",
      "Vertex color: ", ColorSetter[Dynamic[vColor]], "   ",
      "Edge color: ", ColorSetter[Dynamic[eColor]]
    }],
    Row[{
      "Chart color: ", ColorSetter[Dynamic[chartColor, Function[c,
         chartColor = c;
         If[editingChartIndex =!= None, charts[[editingChartIndex, "Color"]] = c]]]],
      "   Chart opacity: ",
      Slider[Dynamic[chartOpacity, Function[o,
         chartOpacity = o;
         If[editingChartIndex =!= None, charts[[editingChartIndex, "Opacity"]] = o]]],
       {0.05, 0.9}], "   Chart radius: ",
      (* Like color and opacity: sets the radius of the next chart, and
         resizes the chart being edited live. *)
      Slider[Dynamic[chartPad, Function[r,
         chartPad = r;
         If[editingChartIndex =!= None, charts[[editingChartIndex, "Radius"]] = r]]],
       {0.1, 2.5}],
      " ", Dynamic[NumberForm[N[chartPad], {3, 2}]]
    }],
    Row[{
      Button["Finish Chart", If[Length[buildingChart] >= 1 || Length[buildingChartEdges] >= 1,
        pushUndo[];
        (* plain Set (not AppendTo) so the canvas Dynamic reliably re-renders *)
        charts = Append[charts, <|"Vertices" -> buildingChart, "Edges" -> buildingChartEdges,
           "Color" -> chartColor, "Opacity" -> chartOpacity, "Radius" -> chartPad|>];
        buildingChart = {}; buildingChartEdges = {}]],
      Button["Cancel Chart", buildingChart = {}; buildingChartEdges = {}],
      Button["Done Editing Chart", editingChartIndex = None]
    }],
    Dynamic[If[Length[charts] > 0,
      Column[{
        Style["Charts (covers):", Bold],
        Column[MapIndexed[
          Function[{b, ix},
            Panel[
             Row[{
               Graphics[{b["Color"], Opacity[b["Opacity"]], Disk[]},
                 ImageSize -> 16],
               "  Chart " <> ToString[First[ix]] <> "  (" <>
                ToString[Length[b["Vertices"]]] <> " vertices, " <>
                ToString[Length[b["Edges"]]] <> " edges)   ",
               Button["Edit", editingChartIndex = First[ix];
                 chartColor = b["Color"]; chartOpacity = b["Opacity"];
                 chartPad = chartRadius[b];
                 mode = "EditChart"],
               Button["Delete", pushUndo[];
                 adjEditIx[First[ix]]; charts = Delete[charts, First[ix]]]
             }],
             Background -> If[First[ix] === editingChartIndex, LightYellow, None],
             FrameMargins -> 3]],
          charts]]
      }],
      ""]],
    Row[{"Import Graph[...] from: ",
      InputField[Dynamic[importExpr], String, ImageSize -> 220],
      Button["Load", Module[{g = Quiet[ToExpression[importExpr]]},
        If[Head[g] === Graph,
         pushUndo[];
         Module[{vl, emb, elist, d},
           vl = VertexList[g];
           emb = N[GraphEmbedding[g]];
           elist = EdgeList[g];
           d = If[Length[emb] > 0, Length[First[emb]], 2];
           vertices = AssociationThread[vl, rescaleEmbedding[emb]];
           vertexColors = Association[Table[
              v -> Replace[Quiet[PropertyValue[{g, v}, VertexStyle]],
                 Automatic | _?FailureQ -> Black], {v, vl}]];
           vertexLabels = Association[Table[
              v -> Module[{lbl = Quiet[PropertyValue[{g, v}, VertexLabels]]},
                 If[StringQ[lbl], lbl, v]], {v, vl}]];
           edges = MapIndexed[
              Function[{e, ix},
               <|"From" -> First[e], "To" -> Last[e],
                 "Directed" -> (Head[e] === DirectedEdge),
                 "Color" -> Replace[Quiet[PropertyValue[{g, e}, EdgeStyle]],
                    Automatic | _?FailureQ -> Black],
                 "Id" -> First[ix]|>],
              elist];
           charts = {}; buildingChart = {}; buildingChartEdges = {};
           selected = None; moving = None;
           dim = d; newZ = 0.;
           nextId = Max[Append[Select[vl, IntegerQ], 0]] + 1;
           nextEdgeId = Length[elist] + 1
         ],
         Print["\"" <> importExpr <> "\" did not evaluate to a Graph[...] object."]]]]
    }],
    Row[{Checkbox[Dynamic[exportLabels]], " Include vertex labels in exported image"}],
    Row[{
      Button["Undo", undo[]], "   ",
      Button["Clear All", pushUndo[]; vertices = <||>; vertexColors = <||>;
        vertexLabels = <||>; edges = {};
        charts = {}; buildingChart = {}; buildingChartEdges = {};
        nextId = 1; nextEdgeId = 1; selected = None; moving = None;
        editingChartIndex = None; edgeAnimA = None],
      Button["Export as Graph[...]",
        Global`LastGraph = Graph[
          Keys[vertices],
          (If[#["Directed"], DirectedEdge[#["From"], #["To"]],
              UndirectedEdge[#["From"], #["To"]]] & /@ edges),
          VertexCoordinates -> Normal[vertices],
          VertexStyle -> Normal[vertexColors],
          EdgeStyle -> ((If[#["Directed"], DirectedEdge[#["From"], #["To"]],
                UndirectedEdge[#["From"], #["To"]]] -> #["Color"]) & /@ edges),
          VertexLabels -> If[exportLabels, Normal[vertexLabels], None], VertexSize -> 0.25];
        Print["Assigned to Global`LastGraph :"];
        Print[Global`LastGraph]],
      Button["Export Cover + Picture",
        Global`LastCover = #["Vertices"] & /@ charts;
        Global`LastCanvasGraphics = If[dim == 3,
          Graphics3D[
           {Table[{Opacity[b["Opacity"]], b["Color"], EdgeForm[None],
              chartTube3D[vertices, b["Vertices"], edges, b["Edges"], chartRadius[b]]},
             {b, charts}],
            Table[{e["Color"],
              edgePrimitive[vertices[e["From"]], vertices[e["To"]], e["Directed"]]},
              {e, edges}],
            KeyValueMap[{vertexColors[#1], Sphere[#2, 0.25],
               If[exportLabels, {Black, Text[Lookup[vertexLabels, #1, #1], #2]}, {}]} &,
              vertices]},
           Boxed -> False, ImageSize -> 500],
          Graphics[
           {Table[{Opacity[b["Opacity"]], b["Color"],
              EdgeForm[{Thin, Opacity[0.9], Darker[b["Color"], 0.3]}],
              chartTube[vertices, b["Vertices"], edges, b["Edges"], chartRadius[b]]}, {b, charts}],
            Table[{e["Color"],
              edgePrimitive[vertices[e["From"]], vertices[e["To"]], e["Directed"]]},
              {e, edges}],
            KeyValueMap[{vertexColors[#1], EdgeForm[Black], Disk[#2, 0.3],
               If[exportLabels, {Black, Text[Lookup[vertexLabels, #1, #1], #2]}, {}]} &,
              vertices]},
           PlotRange -> {{-10, 10}, {-10, 10}}, ImageSize -> 500, Frame -> True]];
        Print["Assigned to Global`LastCover (list of vertex sets) and Global`LastCanvasGraphics :"];
        Print[Global`LastCover];
        Print[Global`LastCanvasGraphics]]
    }],
    (* One canvas, matched to the dimension of the graph: a flat Graphics for
       a 2D graph, and the rotatable Graphics3D for a 3D graph. Both support
       every editing mode, so there is never a second, less useful view
       alongside the one you are actually working in. *)
    Dynamic[If[dim == 2,

     (* ---------------- 2D canvas ---------------- *)
     EventHandler[
      Dynamic[Module[{v2 = projectVertices[dim, vertices]},
        Graphics[
         {
          MapIndexed[
           Function[{b, ix},
            {Opacity[b["Opacity"]], b["Color"],
             If[First[ix] === editingChartIndex,
              EdgeForm[{Thick, Dashed, Black}],
              EdgeForm[{Thin, Opacity[0.9], Darker[b["Color"], 0.3]}]],
             chartTube[v2, b["Vertices"], edges, b["Edges"], chartRadius[b]]}],
           charts],
          If[Length[buildingChart] >= 1 || Length[buildingChartEdges] >= 1,
           {Opacity[0.5*chartOpacity], chartColor, EdgeForm[{Dashed, chartColor}],
            chartTube[v2, buildingChart, edges, buildingChartEdges, chartPad]}, {}],
          Table[
           {e["Color"], edgePrimitive[v2[e["From"]], v2[e["To"]], e["Directed"]]},
           {e, edges}],
          Table[
           If[
            (mode === "BuildChart" && MemberQ[buildingChartEdges, e["Id"]]) ||
             (mode === "EditChart" && editingChartIndex =!= None &&
               MemberQ[charts[[editingChartIndex, "Edges"]], e["Id"]]),
            {Opacity[0.6],
             If[mode === "BuildChart", chartColor, charts[[editingChartIndex, "Color"]]],
             Thickness[0.02], Line[{v2[e["From"]], v2[e["To"]]}]},
            {}],
           {e, edges}],
          If[edgeAnimA =!= None,
           {eColor, Thick, Line[{edgeAnimA, edgeAnimA + edgeAnimT*(edgeAnimB - edgeAnimA)}]},
           {}],
          KeyValueMap[
           Function[{id, pos},
            {vertexColors[id],
             Which[
               id === selected, EdgeForm[{Thick, Red}],
               mode === "BuildChart" && MemberQ[buildingChart, id], EdgeForm[{Thick, chartColor}],
               mode === "EditChart" && editingChartIndex =!= None &&
                 MemberQ[charts[[editingChartIndex, "Vertices"]], id],
                 EdgeForm[{Thick, charts[[editingChartIndex, "Color"]]}],
               True, EdgeForm[Black]],
             Disk[pos, 0.3],
             If[showCanvasLabels, {Black, Text[Lookup[vertexLabels, id, id], pos]}, {}]}],
           v2]
         },
         PlotRange -> {{-10, 10}, {-10, 10}}, ImageSize -> 500,
         Frame -> True, Axes -> False]],
       (* Explicit tracking: without this the canvas can keep showing a stale
          picture after a Button (e.g. "Finish Chart") mutates the state, so a
          just-finished chart would not appear until some other symbol changed. *)
       TrackedSymbols :> {vertices, vertexColors, vertexLabels, edges, charts,
         buildingChart, buildingChartEdges, editingChartIndex, chartColor,
         chartOpacity, chartPad, mode, selected, dim, showCanvasLabels,
         edgeAnimA, edgeAnimB, edgeAnimT}],
      {
       "MouseDown" :> Module[{pos = MousePosition["Graphics"]},
         If[pos =!= None,
          Which[
           mode === "AddVertex",
           pushUndo[];
           vertices[nextId] = If[dim == 3, Append[pos, newZ], pos];
           vertexColors[nextId] = vColor; vertexLabels[nextId] = nextId; nextId++,

           mode === "AddEdge",
           Module[{v2 = projectVertices[dim, vertices], v},
             v = nearestVertex[pos, v2];
             If[v =!= None,
              If[selected === None, selected = v,
               If[v =!= selected,
                pushUndo[];
                animateEdge[v2[selected], v2[v]];
                AppendTo[edges, <|"From" -> selected, "To" -> v,
                  "Directed" -> directedQ, "Color" -> eColor, "Id" -> nextEdgeId|>];
                nextEdgeId++];
               selected = None]]],

           mode === "Move",
           moving = nearestVertex[pos, projectVertices[dim, vertices]];
           If[moving =!= None, pushUndo[]],

           mode === "SetHeight",
           Module[{v = nearestVertex[pos, projectVertices[dim, vertices]]},
             If[v =!= None, pushUndo[]; selected = v; newZ = vertices[v][[3]]]],

           mode === "RecolorVertex",
           Module[{v = nearestVertex[pos, projectVertices[dim, vertices]]},
             If[v =!= None, pushUndo[]; vertexColors[v] = vColor]],

           mode === "LabelVertex",
           Module[{v = nearestVertex[pos, projectVertices[dim, vertices]]},
             If[v =!= None,
              pushUndo[];
              vertexLabels[v] = If[StringTrim[labelText] === "", v, labelText]]],

           mode === "RecolorEdge",
           Module[{v2 = projectVertices[dim, vertices], cand},
             cand = SortBy[Select[edges,
                distToSegment[pos, v2[#["From"]], v2[#["To"]]] < 0.3 &],
               distToSegment[pos, v2[#["From"]], v2[#["To"]]] &];
             If[cand =!= {}, pushUndo[]; edges = ReplaceAll[edges,
                First[cand] -> Append[First[cand], "Color" -> eColor]]]],

           mode === "DeleteVertex",
           Module[{v = nearestVertex[pos, projectVertices[dim, vertices]]},
             If[v =!= None, pushUndo[]; dropVertex[v]]],

           mode === "DeleteEdge",
           Module[{v2 = projectVertices[dim, vertices], cand},
             cand = SortBy[Select[edges,
                distToSegment[pos, v2[#["From"]], v2[#["To"]]] < 0.3 &],
               distToSegment[pos, v2[#["From"]], v2[#["To"]]] &];
             If[cand =!= {}, pushUndo[]; dropEdgeId[First[cand]["Id"]]]],

           mode === "BuildChart",
           Module[{v2 = projectVertices[dim, vertices], v, cand},
             v = nearestVertex[pos, v2];
             If[v =!= None,
              If[MemberQ[buildingChart, v],
               buildingChart = DeleteCases[buildingChart, v],
               AppendTo[buildingChart, v]],
              cand = SortBy[Select[edges,
                 distToSegment[pos, v2[#["From"]], v2[#["To"]]] < 0.3 &],
                distToSegment[pos, v2[#["From"]], v2[#["To"]]] &];
              If[cand =!= {},
               Module[{eid = First[cand]["Id"]},
                If[MemberQ[buildingChartEdges, eid],
                 buildingChartEdges = DeleteCases[buildingChartEdges, eid],
                 AppendTo[buildingChartEdges, eid]]]]]],

           mode === "DeleteChart",
           Module[{v2 = projectVertices[dim, vertices], hit, hitIx},
             hit = SelectFirst[Reverse[charts],
               RegionMember[chartTube[v2, #["Vertices"], edges, #["Edges"], chartRadius[#]], pos] &,
               None];
             If[hit =!= None,
              pushUndo[];
              hitIx = First[FirstPosition[charts, hit]];
              adjEditIx[hitIx];
              charts = DeleteCases[charts, hit]]],

           mode === "EditChart",
           Module[{v2 = projectVertices[dim, vertices], v, cand, hit},
             v = nearestVertex[pos, v2];
             Which[
              v =!= None && editingChartIndex =!= None,
              pushUndo[];
              charts[[editingChartIndex, "Vertices"]] =
               If[MemberQ[charts[[editingChartIndex, "Vertices"]], v],
                DeleteCases[charts[[editingChartIndex, "Vertices"]], v],
                Append[charts[[editingChartIndex, "Vertices"]], v]],

              editingChartIndex =!= None &&
               (cand = SortBy[Select[edges,
                   distToSegment[pos, v2[#["From"]], v2[#["To"]]] < 0.3 &],
                  distToSegment[pos, v2[#["From"]], v2[#["To"]]] &]) =!= {},
              pushUndo[];
              Module[{eid = First[cand]["Id"]},
               charts[[editingChartIndex, "Edges"]] =
                If[MemberQ[charts[[editingChartIndex, "Edges"]], eid],
                 DeleteCases[charts[[editingChartIndex, "Edges"]], eid],
                 Append[charts[[editingChartIndex, "Edges"]], eid]]],

              True,
              hit = SelectFirst[Reverse[Range[Length[charts]]],
                RegionMember[chartTube[v2, charts[[#]]["Vertices"], edges, charts[[#]]["Edges"],
                  chartRadius[charts[[#]]]], pos] &,
                None];
              If[hit =!= None,
               editingChartIndex = hit;
               chartColor = charts[[hit, "Color"]];
               chartOpacity = charts[[hit, "Opacity"]];
               chartPad = chartRadius[charts[[hit]]]]]]
          ]]],
       "MouseDragged" :> If[mode === "Move" && moving =!= None,
         Module[{pos = MousePosition["Graphics"]},
          If[pos =!= None,
           vertices[moving] = If[dim == 3, Append[pos, vertices[moving][[3]]], pos]]]],
       "MouseUp" :> (moving = None),
       "KeyDown" :> If[CurrentValue["EventKey"] === "z" &&
          (MemberQ[CurrentValue["EventModifiers"], "Command"] ||
           MemberQ[CurrentValue["EventModifiers"], "Control"]),
         undo[]]
      }, PassEventsDown -> False],

     (* ---------------- 3D canvas ---------------- *)
     Dynamic[
       EventHandler[
        Graphics3D[
         {
          MapIndexed[
           Function[{b, ix},
            {Opacity[b["Opacity"]], b["Color"],
             If[First[ix] === editingChartIndex, EdgeForm[{Thick, Black}], EdgeForm[None]],
             chartTube3D[vertices, b["Vertices"], edges, b["Edges"], chartRadius[b]]}],
           charts],
          If[Length[buildingChart] >= 1 || Length[buildingChartEdges] >= 1,
           {Opacity[0.5*chartOpacity], chartColor, EdgeForm[None],
            chartTube3D[vertices, buildingChart, edges, buildingChartEdges, chartPad]}, {}],
          Table[{e["Color"],
             edgePrimitive[vertices[e["From"]], vertices[e["To"]], e["Directed"]]},
            {e, edges}],
          (* thick colored tube over edges belonging to the chart in progress *)
          Table[
           If[(mode === "BuildChart" && MemberQ[buildingChartEdges, e["Id"]]) ||
              (mode === "EditChart" && editingChartIndex =!= None &&
                MemberQ[charts[[editingChartIndex, "Edges"]], e["Id"]]),
            {Opacity[0.55],
             If[mode === "BuildChart", chartColor, charts[[editingChartIndex, "Color"]]],
             Tube[{vertices[e["From"]], vertices[e["To"]]}, 0.12]},
            {}],
           {e, edges}],
          KeyValueMap[
           Function[{id, pos},
            {(* halo: red for the vertex picked up by Add Edge / Set Height,
                 chart-coloured for membership of the chart in progress *)
             Which[
              id === selected, {Opacity[0.4], Red, Sphere[pos, 0.45]},
              (mode === "BuildChart" && MemberQ[buildingChart, id]) ||
               (mode === "EditChart" && editingChartIndex =!= None &&
                 MemberQ[charts[[editingChartIndex, "Vertices"]], id]),
              {Opacity[0.4],
               If[mode === "BuildChart", chartColor, charts[[editingChartIndex, "Color"]]],
               Sphere[pos, 0.45]},
              True, {}],
             Opacity[1], vertexColors[id], Sphere[pos, 0.25],
             If[showCanvasLabels, {Black, Text[Lookup[vertexLabels, id, id], pos]}, {}]}],
           vertices]
         },
         (* No bounding box: with the box drawn, rotating spins the box and it
            reads as the camera moving; without it the graph itself appears to
            turn. SphericalRegion keeps the scale fixed while it turns. *)
         Boxed -> False, ImageSize -> 500, SphericalRegion -> True,
         PlotLabel -> If[mode === "Move",
           "3D canvas - drag a vertex to move it in its own z-plane",
           "3D canvas - drag to rotate, click to edit"]],
        {
         (* Click-vs-drag: in every mode but Move, a drag rotates the view
            (PassEventsDown -> True) while a click that barely moved is sent
            to handle3DClick with the mouse ray through the 3D bounding box.
            In Move mode events are held back so a drag moves the vertex
            under the cursor instead of rotating the scene. *)
         "MouseDown" :> (
           mouseDown3D = MousePosition["ScreenAbsolute"];
           If[mode === "Move",
            Module[{ray = MousePosition["Graphics3DBoxIntercepts"], pick},
             pick = If[ray === None, None, pickFromRay[vertices, edges, ray]];
             moving = If[pick =!= None && First[pick] === "Vertex", Last[pick], None];
             If[moving =!= None, pushUndo[]]]]),

         "MouseDragged" :> If[mode === "Move" && moving =!= None,
           Module[{ray = MousePosition["Graphics3DBoxIntercepts"]},
            If[ray =!= None,
             vertices[moving] = rayPlaneZ[ray, vertices[moving][[3]]]]]],

         "MouseUp" :> Module[{now = MousePosition["ScreenAbsolute"], ray},
           If[moving === None && mouseDown3D =!= None && now =!= None &&
              EuclideanDistance[N[now], N[mouseDown3D]] < 4,
            ray = MousePosition["Graphics3DBoxIntercepts"];
            If[ray =!= None, handle3DClick[ray]]];
           moving = None; mouseDown3D = None],

         "KeyDown" :> If[CurrentValue["EventKey"] === "z" &&
            (MemberQ[CurrentValue["EventModifiers"], "Command"] ||
             MemberQ[CurrentValue["EventModifiers"], "Control"]),
           undo[]]
        }, PassEventsDown -> (mode =!= "Move")],
      TrackedSymbols :> {vertices, vertexColors, vertexLabels, edges, charts,
        buildingChart, buildingChartEdges, editingChartIndex, chartColor,
        chartOpacity, chartPad, mode, selected, showCanvasLabels}]],
     TrackedSymbols :> {dim}]
  }]]
];

GraphMaker[]


Rasterize[LastCanvasGraphics, "Image"]
