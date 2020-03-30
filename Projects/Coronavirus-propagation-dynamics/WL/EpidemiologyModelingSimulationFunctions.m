(*
    Epidemiology models simulation functions Mathematica package
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

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)


BeginPackage["EpidemiologyModelingSimulationFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

ModelNDSolveEquations::usage = "ModelNDSolveEquations[model] combines the model equations and initial conditions
into a list equations to be give to NDSolve.";

ModelNDSolve::usage = "ModelNDSolve[model, {t, maxTime}, opts] simulates the model from 0 to maxTime using NDSolve";

MakeHextilingGraph::usage = "MakeHextilingGraph[ aLonLatValue, cellRadius, opts] \
makes a hexagonal tiling graph for specified data.";

Begin["`Private`"];

Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];
Needs["HextileBins`"];

(***********************************************************)
(* ModelNDSolveEquations                                   *)
(***********************************************************)

Clear[ModelNDSolveEquations];

SyntaxInformation[ModelNDSolveEquations] = { "ArgumentsPattern" -> { _ } };

ModelNDSolveEquations::"nargs" = "The first argument is expected to be a model.";

ModelNDSolveEquations[ model_ ] :=
    Block[{lsActualEquations, lsInitialConditions},

      lsInitialConditions =
          Thread[
            Equal[
              model["InitialConditions"][[All, 1]],
              model["InitialConditions"][[All, 2]] //. Join[ToAssociation[model["InitialConditions"]], model["RateRules"]]
            ]
          ];

      lsActualEquations =
          Join[
            model["Equations"] //. Join[Association[ Rule @@@ model["InitialConditions"] ], model["RateRules"]],
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
(* MakeHextilingGraph                                       *)
(***********************************************************)

Clear[MakeHextilingGraph];

MakeHextilingGraph::"nbm" = "The value of the option \"BinMethod\" is expected to be one of \"HextileBins\" or \"GeoHistogram\".";

MakeHextilingGraph::"mr" = "If the value of the option \"BinMethod\" is `1` then the second argument is expected to be `2`.";

Options[MakeHextilingGraph] :=
    Join[
      {"BinMethod" -> "HextileBins", "RemoveLoneCells" -> False},
      Options[HextileBins],
      Options[GeoHistogram],
      Options[NearestNeighborGraph]
    ];

MakeHextilingGraph[
  aLonLatPopulation : Association[({_?NumberQ, _?NumberQ} -> _?NumberQ) ..],
  cellRadius : (_?NumberQ | _Quantity),
  opts : OptionsPattern[] ] :=

    Block[{binMethod, removeLoneCellsQ, grHist, aPolygonValues, lsCells, aCells,
      nc, lsDistances, pos, grHexagonCellsNetwork, grHexagonCells},

      binMethod = OptionValue[MakeHextilingGraph, "BinMethod"];
      removeLoneCellsQ = TrueQ[OptionValue[MakeHextilingGraph, "RemoveLoneCells"]];

      Which[
        ToLowerCase["HextileBins"] == ToLowerCase[binMethod] && NumberQ[cellRadius],
        aPolygonValues = HextileBins[aLonLatPopulation, cellRadius, FilterRules[{opts}, Options[HextileBins]]],

        ToLowerCase["HextileBins"] == ToLowerCase[binMethod] && !NumberQ[cellRadius],
        Message[MakeHextilingGraph::"mr", "\"HextileBins\"", "a number"];
        Return[$Failed],

        ToLowerCase["GeoHistogram"] == ToLowerCase[binMethod] && QuantityQ[cellRadius],
        grHist = GeoHistogram[KeyMap[Reverse, aLonLatPopulation], cellRadius, Automatic, FilterRules[{opts}, Options[GeoHistogram]]];
        aPolygonValues = Association@Cases[grHist[[1]], Tooltip[h_Polygon /; MatrixQ[h[[1]]], pop_ /; NumberQ[pop] && pop > 3] :> h -> pop, \[Infinity]],

        ToLowerCase["GeoHistogram"] == ToLowerCase[binMethod] && !QuantityQ[cellRadius],
        Message[MakeHextilingGraph::"mr", "\"GeoHistogram\"", "Quantity"];
        Return[$Failed],

        True,
        Message[MakeHextilingGraph::"nbm"];
        Return[$Failed]
      ];

      (* Make cell objects *)

      lsCells = KeyValueMap[<|"Value" -> #2, "Cell" -> #1, "Center" -> Mean[PolygonCoordinates[#1]]|> &, aPolygonValues];
      lsCells = SortBy[lsCells, #["Center"] &];
      aCells = AssociationThread[Range[Length[lsCells]], lsCells];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Create a function to find the nearest cesll to a given position *)

      nc = Nearest[Values[aCells] -> Keys[aCells], DistanceFunction -> (EuclideanDistance[#1["Center"], #2["Center"]] &)];

      lsDistances = Select[Flatten@DistanceMatrix[Values[#["Center"] & /@ aCells]], # > 0 &];

      (* identify outlier(s) and drop them *)
      If[removeLoneCellsQ,
        pos = Select[nc[#, {6, 1.1 * Min[lsDistances] / Cos[\[Pi] / 6.]}] & /@ aCells, Length[#] == 1 &];
        aCells = KeyDrop[aCells, Keys[pos]];
      ];

      (* reassign cell ID's *)

      aCells = AssociationThread[Range[Length[aCells]], Values[aCells]];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Make graph *)
      grHexagonCellsNetwork =
          NearestNeighborGraph[
            Keys[aCells], {7, Min[lsDistances] / Cos[\[Pi] / 6.]},
            DistanceFunction -> (EuclideanDistance[aCells[#1]["Center"], aCells[#2]["Center"]] &),
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[NearestNeighborGraph]]
          ];

      grHexagonCells =
          Graph[
            DirectedEdge @@@
                Join[
                  EdgeList[grHexagonCellsNetwork],
                  Reverse /@ EdgeList[grHexagonCellsNetwork]
                ],
            DirectedEdges -> True,
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[Graph]]
          ]
    ];


End[]; (* `Private` *)

EndPackage[]