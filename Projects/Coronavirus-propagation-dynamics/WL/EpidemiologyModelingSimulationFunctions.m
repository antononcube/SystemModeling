(*
    Epidemiology modeling simulation functions Mathematica package
    Copyright (C) 2020  Anton Antonov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Written by Anton Antonov,
    antononcube @ gmail . com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: EpidemiologyModelingSimulationFunctions *)
(* :Context: EpidemiologyModelingSimulationFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-24 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)


(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[EpidemiologyModels`SIRModel]] == 0,
  Echo["EpidemiologyModels.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m"]
];

If[Length[DownValues[EpidemiologyModelModifications`GetStockSymbols]] == 0,
  Echo["EpidemiologyModelModifications.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelModifications.m"]
];

If[Length[DownValues[HextileBins`HextileBins]] == 0,
  Echo["HextileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HextileBins.m"]
];

If[Length[DownValues[TileBins`TileBins]] == 0,
  Echo["TileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/TileBins.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)


BeginPackage["EpidemiologyModelingSimulationFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

ModelNDSolveEquations::usage = "ModelNDSolveEquations[model] combines the model equations and initial conditions
into a list equations to be give to NDSolve.";

ModelNDSolve::usage = "ModelNDSolve[model, {t, maxTime}, opts] simulates the model from 0 to maxTime using NDSolve";

MakePolygonGrid::usage = "MakePolygonGrid[ coords_List, cellSize_?NumberQ, range : ( Automatic | _?MatrixQ), opts___ ] \
makes a polygonal tiling grid for specified data.";

MakeHexagonGrid::usage = "MakeHexagonGrid[ coords_List, cellRadius_?NumberQ, range : ( Automatic | _?MatrixQ), opts___ ] \
makes a hexagonal tiling grid for specified data. \
Shortcut for MakePolygonGrid[ coords, cellSize, \"BinningFunction\" -> HextileBins ]";

ToGraph::usage = "ToGraph[ grid_Association ] makes a graph for a given grid.";

AggregateForCellIDs::usage = "AggregateForCellIDs[ aGrid, aLonLatVal] aggregated the values of aLonLatVal around
the ID's of aGrid[\"Cells\"].";

GridObjectQ::usage = "GridObjectQ[arg] checks is the argument a grid object";

Begin["`Private`"];

Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];
Needs["HextileBins`"];
Needs["TileBins`"];

(***********************************************************)
(* ModelNDSolveEquations                                   *)
(***********************************************************)

Clear[ModelNDSolveEquations];

SyntaxInformation[ModelNDSolveEquations] = { "ArgumentsPattern" -> { _, _. } };

ModelNDSolveEquations::"nargs" = "The first argument is expected to be a model.";

ModelNDSolveEquations[ model_ ] := ModelNDSolveEquations[ model, model["RateRules"] ];

ModelNDSolveEquations[ model_, rateRules_Association ] :=
    Block[{lsActualEquations, lsInitialConditions},

      lsInitialConditions =
          Thread[
            Equal[
              model["InitialConditions"][[All, 1]],
              model["InitialConditions"][[All, 2]] //. Join[ Association[ Rule @@@ model["InitialConditions"] ], rateRules]
            ]
          ];

      lsActualEquations =
          Join[
            model["Equations"] //. Join[ Association[ Rule @@@ model["InitialConditions"] ], rateRules],
            lsInitialConditions
          ];

      lsActualEquations
    ];

ModelNDSolveEquations[___] :=
    Block[{},
      Message[ModelNDSolveEquations::"nargs"];
      $Failed
    ];


(***********************************************************)
(* ModelNDSolve                                            *)
(***********************************************************)

Clear[ModelNDSolve];

SyntaxInformation[ModelNDSolve] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

ModelNDSolve::"nargs" = "The first argument is expected to be a model. \
The second argument is expected to be a time range specification: {var, maxTime}, \
where var is a symbol and maxTime is a positive number.";

ModelNDSolve[ model_?EpidemiologyFullModelQ, {var_, maxTime_?NumericQ}, opts : OptionsPattern[] ] :=
    ModelNDSolve[ model, {var, 0, maxTime}, opts];

ModelNDSolve[ model_?EpidemiologyFullModelQ, {var_, 0, maxTime_?NumericQ}, opts : OptionsPattern[] ] :=
    Block[{lsActualEquations},

      lsActualEquations = ModelNDSolveEquations[ model ];

      NDSolve[lsActualEquations, GetStockSymbols[model], {var, 0, maxTime}, FilterRules[{opts}, NDSolve] ]

    ] /; TrueQ[ Head[var] === Symbol] && maxTime > 0;

ModelNDSolve[___] :=
    Block[{},
      Message[ModelNDSolve::"nargs"];
      $Failed
    ];


(***********************************************************)
(* MakePolygonGrid                                         *)
(***********************************************************)

Clear[GridObjectQ];
GridObjectQ[a_] := AssociationQ[a] && Length[ Intersection[ Keys[a], {"Cells", "AdjacencyMatrix", "Range", "CellRadius"} ] ] == 4;

Clear[MakePolygonGrid];

MakePolygonGrid::nargs = "The first argument is expected to be a numerical matrix with two columns (list of 2D coordinates). \
The second argument is expected to be a number (grid cell size).
The third argument is expected to be a list of two numerical pairs (2D range specification).";

MakePolygonGrid::nbf = "The value of the option \"BinningFunction\" is expected to be one of
\"HextileBins\", \"TileBins\", or Automatic.";

Options[MakePolygonGrid] :=
    Join[
      {"RemoveLoneCells" -> False, "BinningFunction" -> Automatic },
      Options[HextileBins],
      Options[NearestNeighborGraph]
    ];

MakePolygonGridDataQ[d_] := MatrixQ[ d, NumberQ ];

MakePolygonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, opts : OptionsPattern[] ] :=
    MakePolygonGrid[ lsLonLat, cellRadius, Automatic, opts ];

MakePolygonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, Automatic, opts : OptionsPattern[] ] :=
    MakePolygonGrid[ lsLonLat, cellRadius, MinMax /@ Transpose[lsLonLat], opts ];

MakePolygonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, range : { {_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ} }, opts : OptionsPattern[] ] :=
    Block[{removeLoneCellsQ, binningFunction, aPolygonValues, lsCells, aCells,
      nc, lsDistances, pos, grHexagonCellsNetwork, matHexGrid},

      removeLoneCellsQ = TrueQ[OptionValue[MakePolygonGrid, "RemoveLoneCells"]];

      binningFunction = OptionValue[ MakePolygonGrid, "BinningFunction" ];

      Which[

        MemberQ[ {Automatic, "HextileBins", HextileBins}, binningFunction ],
        binningFunction = HextileBins,

        MemberQ[ {"TileBins", TileBins}, binningFunction ],
        binningFunction = TileBins,

        True,
        Message[MakePolygonGrid::nbf];
        Return[$Failed]
      ];

      aPolygonValues = binningFunction[lsLonLat, cellRadius, range, FilterRules[{opts}, Options[binningFunction]]];

      (* Make cell objects *)
      lsCells = KeyValueMap[<|"Value" -> #2, "Cell" -> #1, "Center" -> Mean[PolygonCoordinates[#1]]|> &, aPolygonValues];
      lsCells = SortBy[lsCells, #["Center"] &];
      aCells = AssociationThread[Range[Length[lsCells]], lsCells];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Create a function to find the nearest cell to a given position *)
      nc = Nearest[Values[aCells] -> Keys[aCells], DistanceFunction -> (EuclideanDistance[#1["Center"], #2["Center"]] &)];

      lsDistances = Select[Flatten@DistanceMatrix[Values[#["Center"] & /@ aCells]], # > 0 &];

      (* Identify outlier(s) and drop them *)
      If[removeLoneCellsQ,
        pos = Select[nc[#, {6, 1.1 * Min[lsDistances] / Cos[\[Pi] / 6.]}] & /@ aCells, Length[#] == 1 &];
        aCells = KeyDrop[aCells, Keys[pos]];
      ];

      (* Reassign cell ID's *)
      aCells = AssociationThread[Range[Length[aCells]], Values[aCells]];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Make neighbors graph *)
      (* The NN's radius should fit both hex-tiles and tiles. *)
      grHexagonCellsNetwork =
          NearestNeighborGraph[
            Keys[aCells], {7, Min[lsDistances] / Cos[\[Pi] / 6.]},
            DistanceFunction -> (EuclideanDistance[aCells[#1]["Center"], aCells[#2]["Center"]] &),
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[NearestNeighborGraph]]
          ];

      (* Make final graph matrix *)
      matHexGrid =
          SparseArray[
            Thread[ Rule[ List @@@ Join[EdgeList[grHexagonCellsNetwork], Reverse /@ EdgeList[grHexagonCellsNetwork]], 1 ] ],
            {Length[aCells], Length[aCells]}
          ];

      (* Result *)
      <| "Cells" -> aCells, "AdjacencyMatrix" -> matHexGrid, "Range" -> range, "CellRadius" -> cellRadius |>

    ];

MakePolygonGrid[___] :=
    Block[{},
      Message[MakePolygonGrid::nargs];
      $Failed
    ];


(***********************************************************)
(* MakeHexagonGrid                                         *)
(***********************************************************)

Clear[MakeHexagonGrid];

Options[MakeHexagonGrid] :=
    Join[
      {"RemoveLoneCells" -> False },
      Options[HextileBins],
      Options[NearestNeighborGraph]
    ];

MakeHexagonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, opts : OptionsPattern[] ] :=
    MakeHexagonGrid[ lsLonLat, cellRadius, Automatic, opts ];

MakeHexagonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, Automatic, opts : OptionsPattern[] ] :=
    MakeHexagonGrid[ lsLonLat, cellRadius, MinMax /@ Transpose[lsLonLat], opts ];

MakeHexagonGrid[ lsLonLat_?MakePolygonGridDataQ, cellRadius_?NumberQ, range : { {_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ} }, opts : OptionsPattern[] ] :=
    MakePolygonGrid[ lsLonLat, cellRadius, range, "BinningFunction" -> HextileBins, opts];


(***********************************************************)
(* ToGraph                                                 *)
(***********************************************************)

Clear[ToGraph];

Options[ToGraph] = Options[Graph];

ToGraph[ aGrid_?GridObjectQ, opts : OptionsPattern[] ] :=
    AdjacencyGraph[
      aGrid["AdjacencyMatrix"],
      opts,
      DirectedEdges -> True,
      VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aGrid["Cells"]],
      VertexLabels -> Placed[Automatic, Center],
      VertexSize -> 0.6
    ];


(***********************************************************)
(* AggregateForCellIDs                                     *)
(***********************************************************)

Clear[AggregateForCellIDs];

AggregateForCellIDs::"narg" = "The first argument is expected to be a grid object. \
The second argument is expected to be an association with keys that are coordinates and values that are atoms.";

Options[AggregateForCellIDs] = { "AggregationFunction" -> Total };

AggregateForCellIDs[ aGrid_?GridObjectQ, aLonLatPopulation : Association[ ( {_?NumberQ, _?NumberQ} -> _?AtomQ ) .. ], opts : OptionsPattern[] ] :=
    Block[{nc, aDataIDs, aggrFunc},

      aggrFunc = OptionValue[ AggregateForCellIDs, "AggregationFunction" ];

      nc = Nearest[Values[#["Center"] & /@ aGrid["Cells"]] -> Keys[aGrid["Cells"]]];

      aDataIDs = KeyValueMap[ First[nc[#1, 1]] -> #2 &, aLonLatPopulation ];

      GroupBy[ aDataIDs, First, aggrFunc[ #[[All, 2]] ]& ]
    ];

AggregateForCellIDs[___] :=
    Block[{},
      Message[AggregateForCellIDs::"narg"];
      $Failed
    ];



End[]; (* `Private` *)

EndPackage[]