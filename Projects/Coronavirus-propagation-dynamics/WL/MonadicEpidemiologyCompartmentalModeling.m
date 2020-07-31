(*
    Monadic Epidemiology Compartmental Modeling Mathematica package
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

(* :Title: MonadicEpidemiologyCompartmentalModeling *)
(* :Context: MonadicEpidemiologyCompartmentalModeling` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-04-07 *)
(* Created with the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ . *)

(* :Package Version: 0.3 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:


*)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Echo["StateMonadCodeGenerator.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Echo["CrossTabulate.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[SSparseMatrix`ToSSparseMatrix]] == 0,
  Echo["SSparseMatrix.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
];

If[Length[DownValues[HextileBins`HextileBins]] == 0,
  Echo["HextileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HextileBins.m"]
];

If[Length[DownValues[TileBins`TileBins]] == 0,
  Echo["TileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/TileBins.m"]
];

If[Length[DownValues[EpidemiologyModels`SIRModel]] == 0,
  Echo["EpidemiologyModels.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m"]
];

If[Length[DownValues[EpidemiologyModelModifications`GetStockSymbols]] == 0,
  Echo["EpidemiologyModelModifications.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelModifications.m"]
];

If[Length[DownValues[EpidemiologyModelingSimulationFunctions`ModelNDSolveEquations]] == 0,
  Echo["EpidemiologyModelingSimulationFunctions.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelingSimulationFunctions.m"]
];

If[Length[DownValues[EpidemiologyModelingVisualizationFunctions`EvaluateSolutionsOverGraph]] == 0,
  Echo["EpidemiologyModelingSimulationFunctions.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelingVisualizationFunctions.m"]
];

If[Length[DownValues[SystemDynamicsInteractiveInterfacesFunctions`ParametricSolutionsPlots]] == 0,
  Echo["SystemDynamicsInteractiveInterfacesFunctions.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/WL/SystemDynamicsInteractiveInterfacesFunctions.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicEpidemiologyCompartmentalModeling`"];

CoordinatesToValuesAssociationQ::usage = "Checks does the argument match  <|({_?NumericQ, _?NumericQ} -> _?NumericQ) ...|>.";

BinaryOperateByKeys::usage = "BinaryOperateByKeys[a1_Association, a2_Association, op_] for each key in common applies the binary operation op.";

SubtractByKeys::usage = "SubtractByKeys[a1_Association, a2_Association] for each key in common subtracts the a2 value from the a1 value.";

DeriveSusceptiblePopulation::usage = "DeriveSusceptiblePopulation[total, infected, deceased] \
derives susceptible population from total population, infected, and deceased.";

$ECMMonFailure::usage = "Failure symbol for the monad ECMMon.";

ECMMonGetDefaultModel::usage = "ECMMonGetDefaultModel[] gets the default model in the monad object. \
The default model is determined by the following sequence of checks: \
(1) is the pipeline is an epidemiology model, \
(2) does \"multiSiteModel\" exist, \
(3) does \"singleSiteModel\" exist.";

ECMMonEchoModelGridTableForm::usage = "ECMMonEchoModelGridTableForm[] echoes grid table form of the default model.\
ECMMonEchoModelGridTableForm[spec_String] only echoes the specified parts of the grid table form.";

ECMMonMakePolygonGrid::usage = "ECMMonMakePolygonGrid[points: {{_?NumberQ, _?NumberQ}..} , cellRadius_?NumberQ, opts___] \
creates a hexagonal grid (object) that covers the specified points.";

ECMMonPlotGrid::usage = "ECMMonPlotGrid";

ECMMonPlotGridHistogram::usage = "ECMMonPlotGridHistogram";

ECMMonExtendByGrid::usage = "ECMMonExtendByGrid";

ECMMonExtendByAdjacencyMatrix::usage = "ECMMonExtendByAdjacencyMatrix[ mat_?MatrixQ, factor_?NumberQ] \
extends monad's single site model into multi-site model using the numerical matrix mat.";

ECMMonMakeTravelingPatternsMatrix::usage = "ECMMonMakeTravelingPatternsMatrix[ aOriginDestinationToTravelers_Association, aLocationToLonLat_Association ] \
crates a contingency matrix for the travelers between the cells of grid object in the context.";

ECMMonAssignInitialConditionsByGridAggregation::usage = "ECMMonAssignInitialConditionsByGridAggregation";

ECMMonAssignInitialConditions::usage = "ECMMonAssignInitialConditions";

ECMMonAssignRateRules::usage = "ECMMonAssignRateRules";

ECMMonSimulate::usage = "ECMMonSimulate";

ECMMonBatchSimulate::usage = "ECMMonBatchSimulate";

ECMMonCalibrate::usage = "ECMMonCalibrate";

ECMMonGetSolutionValues::usage = "ECMMonGetSolutionValues[ stockSpec : All | ( _String | {_String..} | _StringExpression ), maxTime_?NumericQ, opts___ ] \
gets the solution values for specified stocks and maximum time.";

ECMMonPlotSolutions::usage = "ECMMonPlotSolutions";

ECMMonPlotSiteSolutions::usage = "ECMMonPlotSiteSolutions";

ECMMonEvaluateSolutionsOverGraph::usage = "ECMMonEvaluateSolutionsOverGraph";

Begin["`Private`"];

Needs["StateMonadCodeGenerator`"];
Needs["CrossTabulate`"];
Needs["SSparseMatrix`"];
Needs["HextileBins`"];
Needs["TileBins`"];
Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];
Needs["EpidemiologyModelingSimulationFunctions`"];
Needs["EpidemiologyModelingVisualizationFunctions`"];
Needs["SystemDynamicsInteractiveInterfacesFunctions`"];


(**************************************************************)
(* Non-monadic functions                                      *)
(**************************************************************)

Clear[CoordinatesToValuesAssociationQ];
CoordinatesToValuesAssociationQ[arg_] := MatchQ[arg, <|({_?NumericQ, _?NumericQ} -> _?NumericQ) ...|>];

Clear[BinaryOperateByKeys];
BinaryOperateByKeys[a1_?AssociationQ, a2_?AssociationQ, oper_] :=
    Merge[{a1, KeyTake[a2, Keys[a1]]}, If[Length[#] > 1, oper[ #[[1]], #[[2]] ], #[[1]]] &];

Clear[SubtractByKeys];
SubtractByKeys[a1_?AssociationQ, a2_?AssociationQ] :=
    Merge[{a1, KeyTake[a2, Keys[a1]]}, If[Length[#] > 1, #[[1]] - #[[2]], #[[1]]] &];

Clear[DeriveSusceptiblePopulation];
DeriveSusceptiblePopulation[
  aPopulations_?CoordinatesToValuesAssociationQ,
  aInfected_?CoordinatesToValuesAssociationQ,
  aDead_?CoordinatesToValuesAssociationQ] :=
    Block[{aRes},
      aRes = SubtractByKeys[aPopulations, aInfected];
      SubtractByKeys[aRes, aDead]
    ];

Clear[StocksSpecQ];
StocksSpecQ[x_] := MatchQ[ x, All | _String | {_String..} | _StringExpression | {_StringExpression ..} ];


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of ECMMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicEpidemiologyCompartmentalModeling`ECMMon", "FailureSymbol" -> $ECMMonFailure, "StringContextNames" -> False ];

(**************************************************************)
(* Setters and takers                                         *)
(**************************************************************)

GenerateMonadAccessors[
  "MonadicEpidemiologyCompartmentalModeling`ECMMon",
  {"singleSiteModel", "multiSiteModel", "grid", "solution", "graph", "adjacencyMatrix" },
  "FailureSymbol" -> $ECMMonFailure ];


(**************************************************************)
(* ECMMonUnit                                                 *)
(**************************************************************)

Clear[ECMMonUnit];

ECMMonUnit[$ECMMonFailure] := $ECMMonFailure;

ECMMonUnit[] := ECMMon[None, Association[]];

ECMMonUnit[{x_, c_Association}] := ECMMon[x, c];

ECMMonUnit[x_] := ECMMon[x, Association[]];

ECMMonUnit[x_?EpidemiologyModelQ] := ECMMonUnit[x, Association[]];

ECMMonUnit[x_, c_Association] := ECMMon[x, c];

ECMMonUnit[x_?EpidemiologyModelQ, c_Association] := ECMMon[x, Join[c, <|"singleSiteModel" -> x|>] ];

ECMMonUnit[___][$ECMMonFailure] := $ECMMonFailure;


(**************************************************************)
(* ECMMonGetDefaultModel                                      *)
(**************************************************************)

Clear[ECMMonGetDefaultModel];

SyntaxInformation[ECMMonGetDefaultModel] = { "ArgumentsPattern" -> {} };

ECMMonGetDefaultModel[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonGetDefaultModel[xs_, context_Association] := ECMMonGetDefaultModel[][xs, context];

ECMMonGetDefaultModel[][xs_, context_] :=
    Block[{model},

      Which[
        EpidemiologyModelQ[xs],
        model = xs,

        KeyExistsQ[context, "multiSiteModel"] && EpidemiologyModelQ[context["multiSiteModel"]],
        model = context["multiSiteModel"],

        KeyExistsQ[context, "singleSiteModel"] && EpidemiologyModelQ[context["singleSiteModel"]],
        model = context["singleSiteModel"],

        True,
        Echo["Cannot find a model.", "ECMMonGetDefaultModel:"];
        Return[$ECMMonFailure]
      ];

      ECMMonUnit[model, context]
    ];

ECMMonGetDefaultModel[__][___] :=
    Block[{},
      Echo["No arguments are expected.", "ECMMonGetDefaultModel:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonEchoModelGridTableForm                               *)
(**************************************************************)

Clear[ECMMonEchoModelGridTableForm];

SyntaxInformation[ECMMonEchoModelGridTableForm] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[ECMMonEchoModelGridTableForm] = Options[ModelGridTableForm];

ECMMonEchoModelGridTableForm[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonEchoModelGridTableForm[xs_, context_Association] := ECMMonEchoModelGridTableForm[][xs, context];

ECMMonEchoModelGridTableForm[][xs_, context_Association] := ECMMonEchoModelGridTableForm[Automatic][xs, context];

ECMMonEchoModelGridTableForm[ opts : OptionsPattern[] ][xs_, context_] :=
    ECMMonEchoModelGridTableForm[Automatic, opts];

ECMMonEchoModelGridTableForm[ spec_, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{model, res},

      model = Fold[ ECMMonBind, ECMMonUnit[xs, context], {ECMMonGetDefaultModel, ECMMonTakeValue}];

      If[ TrueQ[ model === $ECMMonFailure ],
        Return[$ECMMonFailure];
      ];

      Which[
        TrueQ[ spec === Automatic ] || TrueQ[ spec === All ],
        res = ModelGridTableForm[model, opts],

        Length[ Intersection[ Keys[model], Flatten[{spec}] ] ] > 0,
        res = KeyTake[ ModelGridTableForm[model, opts], spec ],

        True,
        Echo["Unknown specification.", "ECMMonEchoModelGridTableForm:"];
        Return[$ECMMonFailure]
      ];

      Echo[res];

      ECMMonUnit[res, context]
    ];

ECMMonEchoModelGridTableForm[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonEchoModelGridTableForm[ spec_, opts___ ]"
            <> " or ECMMonEchoModelGridTableForm[OptionsPattern[]].",
        "ECMMonEchoModelGridTableForm:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonSetInitialConditions                                 *)
(**************************************************************)


(**************************************************************)
(* ECMMonMakePolygonGrid                                      *)
(**************************************************************)

Clear[ECMMonMakePolygonGrid];

SyntaxInformation[ECMMonMakePolygonGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[ECMMonMakePolygonGrid] = { "Coordinates" -> None, "Radius" -> None, "BinningFunction" -> Automatic };

ECMMonMakePolygonGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonMakePolygonGrid[xs_, context_Association] := ECMMonMakePolygonGrid[][xs, context];

ECMMonMakePolygonGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{coords, radius},

      coords = OptionValue[ ECMMonMakePolygonGrid, "Coordinates" ];

      If[ ! MatchQ[coords, { {_?NumericQ, _?NumericQ} .. }],
        Echo[
          "The value of the option \"Coordinates\" is expected to be a list of numeric pairs.",
          "ECMMonMakePolygonGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      radius = OptionValue[ ECMMonMakePolygonGrid, "Radius" ];

      If[ ! ( NumericQ[radius] && radius > 0 ),
        Echo[
          "The value of the option \"Radius\" is expected to be a positive number.",
          "ECMMonMakePolygonGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonMakePolygonGrid[ coords, radius ][xs, context]
    ];

ECMMonMakePolygonGrid[coords : { { _?NumericQ, _?NumericQ } .. }, radius_?NumericQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{aGrid},

      aGrid = MakePolygonGrid[coords, radius, FilterRules[{opts}, Options[MakePolygonGrid]] ];

      ECMMonUnit[aGrid, Join[context, <| "grid" -> aGrid |>]]
    ];

ECMMonMakePolygonGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonMakePolygonGrid[coordinates: { {_?NumericQ, _?NumericQ} .. }, radius_?NumericQ, opts___ ]"
            <> " or ECMMonMakePolygonGrid[OptionsPattern[]].",
        "ECMMonMakePolygonGrid:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonPlotGrid                                             *)
(**************************************************************)

Clear[ECMMonPlotGrid];

SyntaxInformation[ECMMonPlotGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[ECMMonPlotGrid] = Join[ {"Echo" -> True, "CellIDs" -> False }, Options[Graphics] ];

ECMMonPlotGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonPlotGrid[xs_, context_Association] := ECMMonPlotGrid[][xs, context];

ECMMonPlotGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{echoQ, cellIDsQ, gr},

      echoQ = TrueQ[ OptionValue[ECMMonPlotGrid, "Echo"] ];

      cellIDsQ = TrueQ[ OptionValue[ECMMonPlotGrid, "CellIDs"] ];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found.", "ECMMonPlotGrid:"];
        Return[$ECMMonFailure]
      ];

      gr =
          Graphics[
            {
              FaceForm[LightBlue], EdgeForm[Red], Values[Map[#Cell &, context["grid"]["Cells"]]],
              If[ cellIDsQ,
                Values[Map[Text[ #ID, #Center] &, context["grid"]["Cells"]]],
                Nothing
              ]
            },
            FilterRules[ {opts}, Options[Graphics] ],
            ImageSize -> Medium, Frame -> True];

      If[echoQ,
        Echo[ gr, "grid:" ]
      ];

      ECMMonUnit[gr, context]
    ];

ECMMonPlotGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is ECMMonPlotGrid[OptionsPattern[]].",
        "ECMMonPlotGrid:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonPlotGridHistogram                                    *)
(**************************************************************)

Clear[ECMMonPlotGridHistogram];

SyntaxInformation[ECMMonPlotGridHistogram] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonPlotGridHistogram] = Join[ {"Echo" -> True, "ShowDataPoints" -> True }, Options[HextileHistogram] ];

ECMMonPlotGridHistogram[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonPlotGridHistogram[xs_, context_Association] := ECMMonPlotGridHistogram[None][xs, context];

ECMMonPlotGridHistogram[ aData_?CoordinatesToValuesAssociationQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{echoQ, showDataPointsQ, aDataToGrid, histFunc, grHexHist, gr},

      echoQ = TrueQ[ OptionValue[ECMMonPlotGridHistogram, "Echo"] ];

      showDataPointsQ = TrueQ[ OptionValue[ECMMonPlotGridHistogram, "ShowDataPoints"] ];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found.", "ECMMonPlotGridHistogram:"];
        Return[$ECMMonFailure]
      ];

      aDataToGrid = AggregateForCellIDs[ context["grid"], aData];

      histFunc =
          If[ Length[PolygonCoordinates[ context["grid"]["Cells"][[1]]["Cell"]]] == 4,
            TileHistogram,
            (*ELSE*)
            HextileHistogram
          ];

      grHexHist =
          histFunc[ aData, context["grid"]["CellRadius"],
            FilterRules[{opts}, Options[histFunc]],
            ColorFunction -> (Opacity[#, Blue] &),
            PlotRange -> All, ImageSize -> Medium];

      If[ showDataPointsQ,
        gr =
            Show[{
              grHexHist,
              Graphics[{Red, PointSize[0.001 * context["grid"]["CellRadius"]], Point[Keys[aData]]}]
            }],
        (* ELSE *)
        gr = grHexHist
      ];

      If[echoQ,
        Echo[ gr, "grid:" ]
      ];

      ECMMonUnit[gr, context]
    ];

ECMMonPlotGridHistogram[__][___] :=
    Block[{},
      Echo[
        "The expected signature is ECMMonPlotGrid[data : <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, OptionsPattern[]].",
        "ECMMonPlotGridHistogram:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonExtendByGrid                                         *)
(**************************************************************)

Clear[ECMMonExtendByGrid];

SyntaxInformation[ECMMonExtendByGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[ECMMonExtendByGrid] = { "Grid" -> None, "Populations" -> None, "Factor" -> 1 };

ECMMonExtendByGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonExtendByGrid[xs_, context_Association] := ECMMonExtendByGrid[][xs, context];

ECMMonExtendByGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{grid, populations, factor},

      grid = OptionValue[ ECMMonMakePolygonGrid, "Grid" ];

      If[ ! GridObjectQ[grid],
        Echo[
          "The value of the option \"Grid\" is expected to be a grid object. (See GridObjectQ.)",
          "ECMMonExtendByGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      populations = OptionValue[ ECMMonMakePolygonGrid, "Populations" ];

      If[ ! CoordinatesToValuesAssociationQ[populations],
        Echo[
          "The value of the option \"Populations\" is expected to be a association of coordinates to values.",
          "ECMMonExtendByGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      factor = OptionValue[ ECMMonExtendByGrid, "Factor" ];

      If[ ! ( NumericQ[factor] && factor > 0 ),
        Echo[
          "The value of the option \"Factor\" is expected to be a positive number.",
          "ECMMonExtendByGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonExtendByGrid[ grid, factor ][xs, context]
    ];

ECMMonExtendByGrid[ aPopulations_Association, factor_?NumericQ ][xs_, context_] :=
    Block[{},
      If[ KeyExistsQ[context, "grid"],
        ECMMonExtendByGrid[context["grid"], aPopulations, factor][xs, context],
        (* ELSE *)
        ECMMonExtendByGrid[None][xs, context]
      ]
    ];

ECMMonExtendByGrid[aGrid_?GridObjectQ, aPopulations_Association, factor_?NumericQ ][xs_, context_] :=
    Block[{ aICValues, matGridTraffic, singleSiteModel, modelMultiSite},

      (* Check is there are context member gridPopulations. *)
      (* If yes, use the gridPopulations to make the matrix. *)
      aICValues = AggregateForCellIDs[aGrid, aPopulations ];

      matGridTraffic =
          SparseArray[
            Map[ (List @@ #) -> factor * Mean[Map[aICValues[#] &, List @@ #]] &, Most[ArrayRules[aGrid["AdjacencyMatrix"]]][[All, 1]] ],
            Dimensions[aGrid["AdjacencyMatrix"]]
          ];

      If[ !KeyExistsQ[ context, "singleSiteModel"],
        Echo["No single-site, seed model is found. Making one with SEI2RModel.", "ECMMonExtendByGrid:"];
        singleSiteModel = SEI2RModel[Global`t, "InitialConditions" -> True, "RateRules" -> True, "TotalPopulationRepresentation" -> "AlgebraicEquation"],
        (*ELSE*)
        singleSiteModel = context["singleSiteModel"]
      ];

      modelMultiSite = ToSiteCompartmentsModel[ singleSiteModel, matGridTraffic, "MigratingPopulations" -> Automatic];

      ECMMonUnit[modelMultiSite, Join[context, <| "singleSiteModel" -> singleSiteModel, "multiSiteModel" -> modelMultiSite |>]]

    ] /; CoordinatesToValuesAssociationQ[aPopulations];

ECMMonExtendByGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonExtendByGrid[ grid_?GridObjectQ, populations: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, factor_?NumericQ ]"
            <> " or ECMMonExtendByGrid[OptionsPattern[]].",
        "ECMMonExtendByGrid:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonExtendByGraph                                        *)
(**************************************************************)

Clear[ECMMonExtendByGraph];

SyntaxInformation[ECMMonExtendByGraph] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[ECMMonExtendByGraph] = { "Graph" -> None, "Factor" -> 1 };

ECMMonExtendByGraph[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonExtendByGraph[xs_, context_Association] := ECMMonExtendByGraph[][xs, context];

ECMMonExtendByGraph[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{graph, factor},

      graph = OptionValue[ ECMMonExtendByGraph, "Graph" ];

      If[ ! MatrixQ[graph, NumberQ],
        Echo[
          "The value of the option \"Graph\" is expected to be a numerical matrix.",
          "ECMMonExtendByGraph:"
        ];
        Return[$ECMMonFailure]
      ];

      factor = OptionValue[ ECMMonExtendByGraph, "Factor" ];

      If[ ! ( NumericQ[factor] && factor > 0 ),
        Echo[
          "The value of the option \"Factor\" is expected to be a positive number.",
          "ECMMonExtendByGraph:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonExtendByGraph[ graph ][xs, context]
    ];

ECMMonExtendByGraph[ graph_Graph, factor_?NumberQ : 1.0 ][xs_, context_] :=
    Block[{matAdj},

      matAdj = AdjacencyMatrix[graph] * factor;

      Fold[ ECMMonBind, ECMMonUnit[xs, context], { ECMMonExtendByAdjacencyMatrix, ECMMonAddToContext[<|"graph" -> graph|>] }]
    ];

ECMMonExtendByGraph[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonExtendByGraph[ graph_Graph, factor_?NumberQ ]"
            <> " or ECMMonExtendByGraph[OptionsPattern[]].",
        "ECMMonExtendByGraph:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonExtendByAdjacencyMatrix                              *)
(**************************************************************)

Clear[ECMMonExtendByAdjacencyMatrix];

SyntaxInformation[ECMMonExtendByAdjacencyMatrix] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[ECMMonExtendByAdjacencyMatrix] = { "AdjacencyMatrix" -> None, "MigratingPopulations" -> Automatic };

ECMMonExtendByAdjacencyMatrix[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonExtendByAdjacencyMatrix[xs_, context_Association] := ECMMonExtendByAdjacencyMatrix[][xs, context];

ECMMonExtendByAdjacencyMatrix[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{adjacencyMatrix},

      adjacencyMatrix = OptionValue[ ECMMonExtendByAdjacencyMatrix, "AdjacencyMatrix" ];

      If[ ! MatrixQ[adjacencyMatrix, NumberQ],
        Echo[
          "The value of the option \"AdjacencyMatrix\" is expected to be a numerical matrix.",
          "ECMMonExtendByAdjacencyMatrix:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonExtendByAdjacencyMatrix[ adjacencyMatrix, opts ][xs, context]
    ];

ECMMonExtendByAdjacencyMatrix[ matAdj_?MatrixQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{ singleSiteModel, modelMultiSite},

      If[ !KeyExistsQ[ context, "singleSiteModel"],
        Echo["No single-site, seed model is found. Making one with SEI2RModel.", "ECMMonExtendByGrid:"];
        singleSiteModel = SEI2RModel[Global`t, "InitialConditions" -> True, "RateRules" -> True, "TotalPopulationRepresentation" -> "AlgebraicEquation"],
        (*ELSE*)
        singleSiteModel = context["singleSiteModel"]
      ];

      (* Maybe we should also make a corresponding grid object. *)

      modelMultiSite = ToSiteCompartmentsModel[ singleSiteModel, matAdj, FilterRules[{opts}, Options[ToSiteCompartmentsModel]] ];

      ECMMonUnit[modelMultiSite, Join[context, <| "singleSiteModel" -> singleSiteModel, "multiSiteModel" -> modelMultiSite, "adjacencyMatrix" -> matAdj |>]]

    ] /; MatrixQ[matAdj, NumberQ];

ECMMonExtendByAdjacencyMatrix[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonExtendByAdjacencyMatrix[ mat_?MatrixQ ]"
            <> " or ECMMonExtendByAdjacencyMatrix[OptionsPattern[]].",
        "ECMMonExtendByAdjacencyMatrix:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonMakeTravelingPatternsMatrix                          *)
(**************************************************************)

Clear[ECMMonMakeTravelingPatternsMatrix];

SyntaxInformation[ECMMonMakeTravelingPatternsMatrix] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[ECMMonMakeTravelingPatternsMatrix] = { "OriginDestinationToTravelers" -> None, "LocationToLongitudeLatitude" -> None };

ECMMonMakeTravelingPatternsMatrix[___][$ECMMonFailure] := $ECMMonFailure;

(*ECMMonMakeTravelingPatternsMatrix[xs_, context_Association] := $ECMMonFailure;*)

ECMMonMakeTravelingPatternsMatrix[][xs_, context_Association] := $ECMMonFailure;

ECMMonMakeTravelingPatternsMatrix[ aOriginDestinationToTravelers_Association, aLocationToLonLat_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{matAT, aCellIDToLocation, aLocationToCellID, aLonLatToLocation, aOriginDestinationToTravelersByCells, m, n},

      If[ !MatchQ[ aOriginDestinationToTravelers, Association[ ( {_, _} -> _?NumberQ ) .. ] ],
        Echo["The first argument is expected to a be an association that maps origin-destination pairs to values.", "ECMMonMakeTravelingPatternsMatrix:"];
        Return[$ECMMonFailure]
      ];

      If[ !MatchQ[ aLocationToLonLat, Association[ ( _ -> { _?NumberQ, _?NumberQ } ) .. ] ],
        Echo["The second argument is expected to a be an association that location ID's to coordinates (longitude-latitude pairs.)", "ECMMonMakeTravelingPatternsMatrix:"];
        Return[$ECMMonFailure]
      ];

      If[ !KeyExistsQ[context, "grid"],
        Echo["Cannot find grid object.", "ECMMonMakeTravelingPatternsMatrix:"];
        Return[$ECMMonFailure]
      ];

      aLonLatToLocation = Association[ Reverse /@ Normal[aLocationToLonLat] ];

      aCellIDToLocation = AggregateForCellIDs[ context["grid"], aLonLatToLocation, "AggregationFunction" -> Identity];

      aLocationToCellID = Association[Flatten[Thread[Reverse[#]] & /@ Normal[aCellIDToLocation]]];

      (* "Modify the airport-to-airport contingency matrix to cellID-to-cellID ... *)

      aOriginDestinationToTravelersByCells = KeyMap[ # /. aLocationToCellID&, aOriginDestinationToTravelers];

      matAT = CrossTabulate[ Map[ Flatten, List @@@ Normal[aOriginDestinationToTravelersByCells] ], "Sparse" -> True];
      matAT = ToSSparseMatrix[matAT];

      {m, n} = Dimensions[context["grid"]["AdjacencyMatrix"]];
      matAT = ImposeColumnNames[matAT, ToString /@ Range[n]];
      matAT = ImposeRowNames[matAT, ToString /@ Range[m]];

      ECMMonUnit[matAT, context]
    ];

ECMMonMakeTravelingPatternsMatrix[__][__] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonMakeTravelingPatternsMatrix[ _Association, _Association, opts___ ]"
            <> " or ECMMonMakeTravelingPatternsMatrix[OptionsPattern[]].",
        "ECMMonMakeTravelingPatternsMatrix:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonEvaluateSolutionsOverGraph                           *)
(**************************************************************)

Clear[ECMMonEvaluateSolutionsOverGraph];

SyntaxInformation[ECMMonEvaluateSolutionsOverGraph] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[ECMMonEvaluateSolutionsOverGraph] =
    Join[
      { "Stocks" -> "Infected Normally Symptomatic Population", "TimeRange" -> None },
      Options[EvaluateSolutionsOverGraph]
    ];

ECMMonEvaluateSolutionsOverGraph[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonEvaluateSolutionsOverGraph[xs_, context_Association] := ECMMonEvaluateSolutionsOverGraph[][xs, context];

ECMMonEvaluateSolutionsOverGraph[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, timeRange},

      stocks = OptionValue[ ECMMonEvaluateSolutionsOverGraph, "Stocks" ];

      If[ ! MatchQ[ stocks, ( _String | { _String ..} | _StringExpression | { _StringExpression ..} ), population ],
        Echo[
          "The value of the option \"Stocks\" is expected to be a valid stocks specification: " <>
              "( _String | { _String ..} | _StringExpression | { _StringExpression ..} ).",
          "ECMMonEvaluateSolutionsOverGraph:"
        ];
        Return[$ECMMonFailure]
      ];

      timeRange = OptionValue[ ECMMonEvaluateSolutionsOverGraph, "TimeRange" ];

      If[ ! MatchQ[ timeRange, _?NumberQ | {_?NumberQ, _?NumberQ} | {_?NumberQ, _?NumberQ, _?NumberQ} ],
        Echo[
          "The value of the option \"TimeRange\" is expected to be a valid range specification: " <>
              "( maxTime_?NumberQ | {minTime_?NumberQ, maxTime_?NumberQ} | {minTime_?NumberQ, maxTime_?NumberQ, timeStep_?NumberQ} ) .",
          "ECMMonEvaluateSolutionsOverGraph:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonEvaluateSolutionsOverGraph[ population, timeRange, opts ][xs, context]
    ];

ECMMonEvaluateSolutionsOverGraph[ populationSpec_, timeRangeSpec_, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{gr, matAdj, model, sol, res},

      (* Separate checks if grid object is present, or only adjacency matrix is present. *)
      If[ KeyExistsQ[context, "graph"],
        gr = context["graph"],
        (*ELSE*)
        matAdj = ECMMonBind[ ECMMonUnit[xs, context], ECMMonTakeAdjacencyMatrix ];
        Print[matAdj];
        If[ !MatrixQ[ matAdj ],
          Echo["Cannot find graph or adjacency matrix.", "ECMMonEvaluateSolutionsOverGraph:" ];
          Return[$ECMMonFailure]
        ];

        gr = AdjacencyGraph[ Unitize[matAdj] ];

        Echo[
          "Using a graph made from the unitized version of the adjacency matrix in the context.",
          "ECMMonEvaluateSolutionsOverGraph:"
        ]
      ];

      If[ KeyExistsQ[context, "multiSiteModel"],
        model = context["multiSiteModel"],
        (*ELSE*)
        Echo["Cannot find multi-site model.", "ECMMonEvaluateSolutionsOverGraph:" ];
        Return[$ECMMonFailure]
      ];

      If[ KeyExistsQ[context, "solution"],
        sol = context["solution"],
        (*ELSE*)
        Echo["Cannot find solution.", "ECMMonEvaluateSolutionsOverGraph:" ];
        Return[$ECMMonFailure]
      ];

      res = EvaluateSolutionsOverGraph[ gr, model, populationSpec, sol, timeRangeSpec, FilterRules[ {opts}, Options[EvaluateSolutionsOverGraph] ] ];

      ECMMonUnit[res, context]
    ];

ECMMonEvaluateSolutionsOverGraph[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonEvaluateSolutionsOverGraph[  ]"
            <> " or ECMMonEvaluateSolutionsOverGraph[OptionsPattern[]].",
        "ECMMonEvaluateSolutionsOverGraph:"];
      $ECMMonFailure
    ];



(**************************************************************)
(* ECMMonEvaluateSolutionsOverGrid                            *)
(**************************************************************)

Clear[ECMMonEvaluateSolutionsOverGrid];

SyntaxInformation[ECMMonEvaluateSolutionsOverGrid] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[ECMMonEvaluateSolutionsOverGrid] = Options[ECMMonGetSolutionValues];

ECMMonEvaluateSolutionsOverGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonEvaluateSolutionsOverGrid[xs_, context_Association] := ECMMonEvaluateSolutionsOverGrid[][xs, context];


ECMMonEvaluateSolutionsOverGrid[ populationSpec_, timeRangeSpec_, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},

      (*      res = *)

      ECMMonUnit[res, context]
    ];

ECMMonEvaluateSolutionsOverGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonEvaluateSolutionsOverGrid[  ]"
            <> " or ECMMonEvaluateSolutionsOverGrid[OptionsPattern[]].",
        "ECMMonEvaluateSolutionsOverGrid:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonAssignInitialConditionsByGridAggregation             *)
(**************************************************************)

Clear[ECMMonAssignInitialConditionsByGridAggregation];

SyntaxInformation[ECMMonAssignInitialConditionsByGridAggregation] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[ECMMonAssignInitialConditionsByGridAggregation] = { "Default" -> 0 };

ECMMonAssignInitialConditionsByGridAggregation[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonAssignInitialConditionsByGridAggregation[xs_, context_Association] := ECMMonAssignInitialConditionsByGridAggregation[][xs, context];

ECMMonAssignInitialConditionsByGridAggregation[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "ECMMonAssignInitialConditions:"];
      $ECMMonFailure
    ];

ECMMonAssignInitialConditionsByGridAggregation[ aCoordsToValues_Association, stockName_String, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{default, modelMultiSite, aICValues, stockSymbol},

      default = OptionValue[ECMMonAssignInitialConditionsByGridAggregation, "Default"];

      If[ ! ( NumericQ[default] && default >= 0 ),
        Echo["The value of the option \"Default\" is expected to be a non-negative number.", "ECMMonAssignInitialConditionsByGridAggregation:"];
        Return[$ECMMonFailure]
      ];

      If[ !KeyExistsQ[ context, "multiSiteModel"],
        Echo["No multi-site model is found. (Required for this signature.)", "ECMMonAssignInitialConditionsByGridAggregation:"];
        Return[$ECMMonFailure]
      ];

      modelMultiSite = context["multiSiteModel"];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found. (Required for this signature.)", "ECMMonAssignInitialConditionsByGridAggregation:"];
        Return[$ECMMonFailure]
      ];

      If[ ! MemberQ[ Values[modelMultiSite["Stocks"]], stockName ],
        Echo["The second argument, \"" <> stockName <> "\", is expected to be one of the stock names: " <> ToString[Union[Values[modelMultiSite["Stocks"]]]],
          "ECMMonAssignInitialConditions:"];
        Return[$ECMMonFailure]
      ];

      If[ Length[aCoordsToValues] > 0,

        aICValues = AggregateForCellIDs[context["grid"], aCoordsToValues ];

        stockSymbol = Head@First@GetStockSymbols[ modelMultiSite, stockName];

        modelMultiSite =
            SetInitialConditions[
              modelMultiSite,
              Join[Association@Map[#[0] -> default &, GetPopulationSymbols[modelMultiSite, stockName]], Association[KeyValueMap[With[{sh = stockSymbol}, sh[#1][0] -> #2 ]&, aICValues]]]
            ],
        (* ELSE *)
        modelMultiSite =
            SetInitialConditions[
              modelMultiSite,
              Association@Map[#[0] -> default &, GetPopulationSymbols[modelMultiSite, stockName]]
            ]
      ];

      ECMMonUnit[modelMultiSite, Join[context, <| "multiSiteModel" -> modelMultiSite |>]]

    ] /; CoordinatesToValuesAssociationQ[aCoordsToValues];

ECMMonAssignInitialConditionsByGridAggregation[__][___] :=
    Block[{},
      Echo[
        "The expected signature is ECMMonAssignInitialConditionsByGridAggregation[ coordsToValues: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, stockName_String, opts ].",
        "ECMMonAssignInitialConditionsByGridAggregation:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonAssignInitialConditions                              *)
(**************************************************************)

Clear[ECMMonAssignInitialConditions];

SyntaxInformation[ECMMonAssignInitialConditions] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[ECMMonAssignInitialConditions] = Options[ECMMonAssignInitialConditionsByGridAggregation];

ECMMonAssignInitialConditions[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonAssignInitialConditions[xs_, context_Association] := ECMMonAssignInitialConditions[][xs, context];

ECMMonAssignInitialConditions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "ECMMonAssignInitialConditions:"];
      $ECMMonFailure
    ];

ECMMonAssignInitialConditions[ stockVals : { _Rule .. }, opts : OptionsPattern[] ][xs_, context_] :=
    ECMMonAssignInitialConditions[ Association[stockVals], opts ][xs, context];

ECMMonAssignInitialConditions[ stockVal_Rule, opts : OptionsPattern[] ][xs_, context_] :=
    ECMMonAssignInitialConditions[ <|stockVal|>, opts ][xs, context];

ECMMonAssignInitialConditions[ aStockValues_Association ][xs_, context_] :=
    Block[{ model, aContextAddition },

      (* Maybe we should be able to use ECMMonGetDefaultModel here. *)
      Which[

        KeyExistsQ[ context, "multiSiteModel"],
        model = context["multiSiteModel"];
        model = AssignInitialConditions[ model, aStockValues ];
        aContextAddition = <| "multiSiteModel" -> model |>,

        KeyExistsQ[ context, "singleSiteModel"],
        model = context["singleSiteModel"];
        model = AssignInitialConditions[ model, aStockValues ];
        aContextAddition = <| "singleSiteModel" -> model |>,

        True,
        Echo["Cannot find a model.", "ECMMonAssignInitialConditions:"];
        Return[$ECMMonFailure]
      ];

      ECMMonUnit[model, Join[context, aContextAddition]]

    ];

ECMMonAssignInitialConditions[ aCoordsToValues_Association, stockName_String, opts : OptionsPattern[] ][xs_, context_] :=
    ECMMonAssignInitialConditionsByGridAggregation[ aCoordsToValues, stockName, opts ][xs, context];

ECMMonAssignInitialConditions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of " <>
            "ECMMonAssignInitialConditions[ aStockValues_Association] or " <>
            "ECMMonAssignInitialConditions[ coordsToValues: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, stockName_String, opts ].",
        "ECMMonAssignInitialConditions:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonAssignRateRules                                      *)
(**************************************************************)

Clear[ECMMonAssignRateRules];

SyntaxInformation[ECMMonAssignRateRules] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

ECMMonAssignRateRules[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonAssignRateRules[xs_, context_Association] := ECMMonAssignRateRules[][xs, context];

ECMMonAssignRateRules[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "ECMMonAssignInitialConditions:"];
      $ECMMonFailure
    ];

ECMMonAssignRateRules[ rateVals : { _Rule .. }, opts : OptionsPattern[] ][xs_, context_] :=
    ECMMonAssignRateRules[ Association[rateVals], opts ][xs, context];

ECMMonAssignRateRules[ rateVal_Rule, opts : OptionsPattern[] ][xs_, context_] :=
    ECMMonAssignRateRules[ <|rateVal|>, opts ][xs, context];

ECMMonAssignRateRules[ aRateRules_Association ][xs_, context_] :=
    Block[{ model, aContextAddition },

      Which[

        KeyExistsQ[ context, "multiSiteModel"],
        model = context["multiSiteModel"];
        model = AssignRateRules[ model, aRateRules ];
        aContextAddition = <| "multiSiteModel" -> model |>,

        KeyExistsQ[ context, "singleSiteModel"],
        model = context["singleSiteModel"];
        model = AssignRateRules[ model, aRateRules ];
        aContextAddition = <| "singleSiteModel" -> model |>,

        True,
        Echo["Cannot find a model.", "ECMMonAssignRateRules:"];
        Return[$ECMMonFailure]
      ];

      ECMMonUnit[model, Join[context, aContextAddition]]

    ];

ECMMonAssignRateRules[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonAssignRateRules[ aRateRules_Association].",
        "ECMMonAssignRateRules:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonSimulate                                             *)
(**************************************************************)

Clear[ECMMonSimulate];

SyntaxInformation[ECMMonSimulate] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonSimulate] = Join[ { "MaxTime" -> 365, "TimeVariable" -> Automatic }, Options[NDSolve] ];

ECMMonSimulate[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonSimulate[xs_, context_Association] := ECMMonAssignInitialConditionsByGridAggregation[][xs, context];

ECMMonSimulate[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{time},

      time = OptionValue[ECMMonSimulate, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo["The value of the option \"MaxTime\" is expected to be a non-negative number.", "ECMMonSimulate:"];
        Return[$ECMMonFailure]
      ];

      ECMMonSimulate[ time, opts][xs, context]
    ];

ECMMonSimulate[ maxTime_?NumericQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{aSol, model, timeVar},

      timeVar = OptionValue[ECMMonSimulate, "TimeVariable"];
      If[ TrueQ[ timeVar === Automatic ], timeVar = Global`t];
      If[ ! Developer`SymbolQ[timeVar],
        Echo["The value of the option \"TimeVariable\" is expected to be a symbol.", "ECMMonSimulate:"];
        Return[$ECMMonFailure]
      ];

      If[ ! ( NumericQ[maxTime] && maxTime >= 0 ),
        Echo["The first argument is expected to be a non-negative number.", "ECMMonSimulate:"];
        Return[$ECMMonFailure]
      ];

      model = Fold[ ECMMonBind, ECMMonUnit[xs, context], {ECMMonGetDefaultModel, ECMMonTakeValue}];

      If[ TrueQ[ model === $ECMMonFailure ],
        Echo["Cannot find a model.", "ECMMonSimulate:"];
        Return[$ECMMonFailure];
      ];

      aSol = Association @ First @ ModelNDSolve[ model, {timeVar, 0, maxTime}, FilterRules[{opts}, Options[NDSolve]] ];

      If[ !( KeyExistsQ[context, "singleSiteModel"] || KeyExistsQ[context, "multiSiteModel" ] ),
        ECMMonUnit[aSol, Join[ context, <| "singleSiteModel" -> model, "solution" -> aSol |>]],
        (* ELSE *)
        ECMMonUnit[aSol, Join[ context, <| "solution" -> aSol |>]]
      ]
    ];

ECMMonSimulate[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonSimulate[ maxTime_?NumericQ, opts___ ].",
        "ECMMonSimulate:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonGetSolutionValues                                    *)
(**************************************************************)

Clear[TimeSpecQ];
TimeSpecQ[spec_] :=
    ( NumericQ[spec] && spec >= 0 ) || MatchQ[spec, {_?NumericQ}] && spec[[1]] >= 0 ||
        ( MatchQ[spec, {_?NumericQ, _?NumericQ}] || MatchQ[spec, {_?NumericQ, _?NumericQ, _?NumericQ}] ) && spec[[1]] <= spec[[2]];

Clear[ToTimeRangeSpec];
ToTimeRangeSpec[timeSpec_?TimeSpecQ] :=
    Switch[timeSpec,

      _?NumericQ, {0, timeSpec},

      {_?NumericQ}, {timeSpec[[1]], timeSpec[[1]]},

      _, timeSpec
    ];

Clear[ECMMonGetSolutionValues];

SyntaxInformation[ECMMonGetSolutionValues] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[ECMMonGetSolutionValues] = { "Stocks" -> All, "TimeSpecification" -> 365 };

ECMMonGetSolutionValues[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonGetSolutionValues[xs_, context_Association] := ECMMonGetSolutionValues[][xs, context];

ECMMonGetSolutionValues[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, time},

      stocks = OptionValue[ECMMonGetSolutionValues, "Stocks"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _Symbol | {_Symbol..} ) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: All | ( _String | {_String..} | _Symbol | {_Symbol..} ).",
          "ECMMonGetSolutionValues:"];
        Return[$ECMMonFailure]
      ];

      time = OptionValue[ECMMonGetSolutionValues, "TimeSpecification"];

      If[ ! TimeSpecQ[time],
        Echo[
          "The value of the option \"TimeSpecification\" is expected to be a non-negative number (corresponding to max time) or a time range specification: " <>
              "( {minTime_?NumberQ, maxTime_?NumberQ} || {minTime_?NumberQ, maxTime_?NumberQ, step_?NumberQ} ).",
          "ECMMonGetSolutionValues:"];
        Return[$ECMMonFailure]
      ];

      ECMMonGetSolutionValues[ stocks, time, opts][xs, context]
    ];

ECMMonGetSolutionValues[
  stocksSpecArg : All | ( _String | {_String..} | _StringExpression | _Symbol | {_Symbol..} ),
  timeSpec_?TimeSpecQ,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{stocksSpec = Flatten[{stocksSpecArg}], multiSiteQ, lsTimes, res},

      Which[
        KeyExistsQ[ context, "multiSiteModel"],
        multiSiteQ = True,

        KeyExistsQ[ context, "singleSiteModel"],
        multiSiteQ = False,

        True,
        Echo["Cannot find a model.", "ECMMonGetSolutionValues:"];
        Return[$ECMMonFailure]
      ];

      If[ ! KeyExistsQ[ context, "solution"],
        Echo["Cannot find solution.", "ECMMonGetSolutionValues:"];
        Return[$ECMMonFailure]
      ];

      (* Shouldn't this be handled by GetStocks? *)
      Which[
        multiSiteQ && TrueQ[stocksSpecArg === All],
        stocksSpec = Union[ Values[context["multiSiteModel"]["Stocks"]] ],

        multiSiteQ && MatchQ[stocksSpec, {_StringExpression..} | {_Symbol..}],
        stocksSpec = GetStocks[ context["multiSiteModel"], stocksSpec ] /. context["multiSiteModel"]["Stocks"],

        !multiSiteQ && TrueQ[stocksSpecArg === All],
        stocksSpec = Union[ Values[context["singleSiteModel"]["Stocks"]] ],

        !multiSiteQ && MatchQ[stocksSpec, {_StringExpression..} | {_Symbol..}],
        stocksSpec = GetStocks[ context["singleSiteModel"], stocksSpec ] /. context["singleSiteModel"]["Stocks"]
      ];

      If[ multiSiteQ,
        res = Association @ Map[ # -> EvaluateSolutionsByModelIDs[context["multiSiteModel"], #, context["solution"], ToTimeRangeSpec[timeSpec] ]&, stocksSpec],

        (*ELSE*)
        lsTimes = Range @@ ToTimeRangeSpec[timeSpec];

        stocksSpec = Union @ Flatten @ Map[ GetStockSymbols[context["singleSiteModel"], #]&, stocksSpec ];

        res = Map[ #[ lsTimes ]&, KeyTake[ context["solution"], stocksSpec ] ];
      ];

      ECMMonUnit[ res, context ]
    ];

ECMMonGetSolutionValues[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonGetSolutionValues[ stockSpec : All | ( _String | {_String..} | _StringExpression ), timeSpec : { _?NumericQ | {minTime_?NumberQ, maxTime_?NumberQ, step_?NumberQ} }, opts___ ]" <>
            " or ECMMonGetSolutionValues[opts___].",
        "ECMMonGetSolutionValues:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonBatchSimulate                                        *)
(**************************************************************)

Clear[ECMMonBatchSimulate];

SyntaxInformation[ECMMonBatchSimulate] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonBatchSimulate] =
    Union @ Join[
      { "Stocks" -> All, "Parameters" -> All, "MaxTime" -> 365 },
      Options[ECMMonSimulate]
    ];

ECMMonBatchSimulate[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonBatchSimulate[xs_, context_Association] := ECMMonBatchSimulate[][xs, context];

ECMMonBatchSimulate[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, params, time},

      stocks = OptionValue[ECMMonBatchSimulate, "Stocks"];

      If[ ! StocksSpecQ[stocks],
        Echo[
          "The value of the option \"Stocks\" is expected to be a string, a string pattern, a list of strings, or a list of string patterns.",
          "ECMMonBatchSimulate:"];
        Return[$ECMMonFailure]
      ];

      params = OptionValue[ECMMonBatchSimulate, "Parameters"];

      If[ ! MatchQ[ params, { _Association .. } | <| ( _ -> { _?NumberQ .. } ) .. |> ],
        Echo[
          "The value of the option \"Parameters\" is expected to be a list of associations or an association of parameter numerical lists.",
          "ECMMonBatchSimulate:"];
        Return[$ECMMonFailure]
      ];

      time = OptionValue[ECMMonBatchSimulate, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo[
          "The value of the option \"MaxTime\" is expected to be a non-negative number.",
          "ECMMonBatchSimulate:"];
        Return[$ECMMonFailure]
      ];

      ECMMonBatchSimulate[ stocks, params, time, opts][xs, context]
    ];

ECMMonBatchSimulate[
  stockSpec_?StocksSpecQ,
  params : <| ( _ -> { _?NumericQ .. } ) .. |>,
  maxTime_?NumericQ,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{lsProcessedParams},

      (* Here we can / should are the parameters known in the model. *)
      (* Not necessary, because unknown rates are going to be ignored. *)
      lsProcessedParams = Flatten @ Outer[AssociationThread[Keys[params], List[##]] &, Sequence @@ Values[params]];

      ECMMonBatchSimulate[ stockSpec, lsProcessedParams, maxTime, opts ][xs, context]

    ];

ECMMonBatchSimulate[
  stockSpec_?StocksSpecQ,
  params : { _Association .. },
  maxTime_?NumericQ,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{res},

      res =
          Association @
              Map[
                Function[{p},
                  p ->
                      Fold[
                        ECMMonBind,
                        ECMMonUnit[xs, context],
                        {
                          ECMMonAssignRateRules[ p ],
                          ECMMonSimulate[maxTime, FilterRules[{opts}, Options[ECMMonSimulate]] ],
                          ECMMonGetSolutionValues[ stockSpec, maxTime ],
                          ECMMonTakeValue
                        }
                      ]
                ],
                params
              ];

      ECMMonUnit[ res, context ]
    ];

ECMMonBatchSimulate[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of" <>
            " ECMMonBatchSimulate[ stockSpec : ( All | _String | {_String..} | _StringExpression | {_StringExpression ..} ), params : { _Association .. } | <| ( _ -> { _?NumberQ ..} ) .. |>, maxTime_?NumericQ, opts___ ]" <>
            " or ECMMonBatchSimulate[opts___].",
        "ECMMonBatchSimulate:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonCalibrate                                        *)
(**************************************************************)

Clear[ECMMonCalibrate];

SyntaxInformation[ECMMonCalibrate] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[ECMMonCalibrate] =
    Join[
      { "Target" -> None, "Parameters" -> All, "StockWeights" -> Automatic, DistanceFunction -> EuclideanDistance },
      { Method -> {"NelderMead", "PostProcess" -> False} },
      Options[ECMMonSimulate],
      DeleteCases[ Options[NMinimize], Method -> _ ]
    ];

ECMMonCalibrate[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonCalibrate[xs_, context_Association] := ECMMonCalibrate[][xs, context];

ECMMonCalibrate[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{target, params},

      target = OptionValue[ECMMonCalibrate, "Target"];

      If[ ! MatchQ[ target, <| ( _ -> { _?NumberQ .. } ) .. |> ],
        Echo[
          "The value of the option \"Target\" is expected to be a list of associations or an association of stocks numerical lists.",
          "ECMMonBatchSimulate:"];
        Return[$ECMMonFailure]
      ];

      params = OptionValue[ECMMonCalibrate, "Parameters"];

      If[ ! MatchQ[ params, { _Association .. } | <| ( _ -> { _?NumberQ .. } ) .. |> ],
        Echo[
          "The value of the option \"Parameters\" is expected to be a list of associations or an association of parameter numerical lists.",
          "ECMMonCalibrate:"];
        Return[$ECMMonFailure]
      ];

      ECMMonCalibrate[ target, params, opts][xs, context]
    ];

ECMMonCalibrate[
  targetArg : <| ( _ -> _?VectorQ ) .. |>,
  params : <| ( _ -> { _?NumericQ .. } ) .. |>,
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{ target = targetArg,
      model, distFunc, stockWeights,
      maxTime, ecmObj, modelEvalFunc, objFunc,
      lsProcessedParams, lsTargetStocks, res},

      (*-----------------------------------------------------*)

      distFunc = OptionValue[ECMMonCalibrate, DistanceFunction];

      stockWeights = OptionValue[ECMMonCalibrate, "StockWeights" ];
      If[ !( TrueQ[ stockWeights === Automatic ] || AssociationQ[stockWeights] ),
        Echo[ "The value of the options \"StockWeights\" expected to be an association or Automatic.", "ECMMonCalibrate:"];
        Return[$ECMMonFailure]
      ];

      (*-----------------------------------------------------*)

      model = Fold[ ECMMonBind, ECMMonUnit[xs, context], {ECMMonGetDefaultModel, ECMMonTakeValue}];

      If[ TrueQ[ model === $ECMMonFailure ],
        Return[$ECMMonFailure];
      ];

      target = KeyTake[ target, Join[ GetStocks[model], GetStockSymbols[model]] ];
      If[ Length[target] == 0,
        Echo[ "None of the keys of the first argument are known stocks.", "ECMMonCalibrate:"];
        Return[$ECMMonFailure]
      ];

      If[ Length[target] < Length[targetArg],
        Echo[ "Some of the keys of the first argument are not known stocks.", "ECMMonCalibrate:"];
      ];

      If[ !( Apply[Equal, Map[ Length, Values[target] ] ] && Apply[ And, VectorQ[#, NumericQ]& /@ Values[target] ] ),
        Echo[
          "The first argument is expected to be an association of target numerical vectors that have same length.",
          "ECMMonCalibrate:"
        ];
        Return[$ECMMonFailure]
      ];

      (*-----------------------------------------------------*)
      If[ TrueQ[stockWeights === Automatic],
        stockWeights = <||>
      ];
      stockWeights = Join[ AssociationThread[ Keys[target], 1 ], stockWeights ];

      maxTime = Length[target[[1]]] - 1; (* Time starts from 0.*)

      ecmObj = ECMMonUnit[xs, context];

      lsTargetStocks = Keys[target];

      modelEvalFunc[x_?NumberQ, args___] :=
          Block[{p},
            p = AssociationThread[ Keys[params], {x, args}];

            Fold[
              ECMMonBind,
              ecmObj,
              {
                ECMMonAssignRateRules[ p ],
                ECMMonSimulate[maxTime, FilterRules[{opts}, Options[ECMMonSimulate]] ],
                ECMMonGetSolutionValues[ lsTargetStocks, maxTime ],
                ECMMonTakeValue
              }
            ]
          ];

      stockWeights = KeyTake[stockWeights, Keys[target]];

      objFunc[x_?NumberQ, args___] :=
          Block[{res},
            res = modelEvalFunc[x, args];
            Dot[
              Values[stockWeights],
              MapThread[ distFunc, { Values @ KeyTake[res, Keys[target] ], Values[target] } ]
            ]
          ];

      (*-----------------------------------------------------*)

      lsProcessedParams = KeyValueMap[ Min[#2] <= #1 <= Max[#2]&, params ];

      res = NMinimize[ { objFunc[Evaluate[Sequence @@ Keys[params]]], Sequence @@ lsProcessedParams }, Keys[params], FilterRules[ {opts}, Options[NMinimize] ] ];

      ECMMonUnit[res, context]
    ];


ECMMonCalibrate[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of" <>
            " ECMMonCalibrate[ target_Association, params : { _Association .. } | <| ( _ -> { _?NumberQ ..} ) .. |>, maxTime_?NumericQ, opts___ ]" <>
            " or ECMMonCalibrate[opts___].",
        "ECMMonCalibrate:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonPlotSolutions                                        *)
(**************************************************************)

Clear[ECMMonPlotSolutions];

SyntaxInformation[ECMMonPlotSolutions] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonPlotSolutions] =
    Join[
      { "Stocks" -> All, "MaxTime" -> Automatic, "Echo" -> True },
      Options[MultiSiteModelStocksPlot],
      Options[ParametricSolutionsPlots]
    ];

ECMMonPlotSolutions[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonPlotSolutions[xs_, context_Association] := ECMMonPlotSolutions[][xs, context];

ECMMonPlotSolutions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, time},

      stocks = OptionValue[ECMMonPlotSolutions, "Stocks"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _StringExpression) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ).",
          "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];

      time = OptionValue[ECMMonPlotSolutions, "MaxTime"];

      If[ ! ( TrueQ[time === Automatic] || NumericQ[time] && time >= 0 ),
        Echo[
          "The value of the option \"MaxTime\" is expected to be a non-negative number otr Automatic.",
          "ECMMonPlotSolutions:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonPlotSolutions[ stocks, time, opts ][xs, context]
    ];

ECMMonPlotSolutions[
  stocksSpecArg : ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ),
  maxTime : ( Automatic | _?NumericQ ),
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{stocksSpec = Flatten[{stocksSpecArg}], echoQ, res, stockSymbols},

      echoQ = TrueQ[ OptionValue[ECMMonPlotSolutions, "Echo"] ];

      If[ ! ( TrueQ[maxTime === Automatic] || NumericQ[maxTime] && maxTime >= 0 ),
        Echo[
          "The second argument is expected to be Automatic or a non-negative number.",
          "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];


      Which[

        (*---------*)
        KeyExistsQ[ context, "multiSiteModel"] && KeyExistsQ[ context, "solution"],

        If[TrueQ[ stocksSpec === {All} ],
          stocksSpec = {__ ~~ ___};
        ];

        res =
            MultiSiteModelStocksPlot[ context["multiSiteModel"], stocksSpec, context["solution"], maxTime,
              FilterRules[{opts}, Options[MultiSiteModelStocksPlot]],
              ImageSize -> Medium, PlotTheme -> "Detailed"
            ],

        (*---------*)
        KeyExistsQ[ context, "singleSiteModel"] && KeyExistsQ[ context, "solution"],

        Which[
          TrueQ[ stocksSpec === {All} ],
          stockSymbols = GetStockSymbols[ context["singleSiteModel"] ],

          MatchQ[ stocksSpec, {_String ..} | {_StringExpression ..} ],
          stockSymbols = Flatten[ GetStockSymbols[ context["singleSiteModel"], # ]& /@ stocksSpec ],

          True,
          stockSymbols = GetStockSymbols[ context["singleSiteModel"] ]
        ];

        res =
            ParametricSolutionsPlots[ context["singleSiteModel"]["Stocks"], KeyTake[context["solution"], stockSymbols], None, maxTime,
              FilterRules[{opts}, Options[ParametricSolutionsPlots]],
              "Together" -> True, ImageSize -> Medium, PlotTheme -> "Detailed"
            ],

        (*---------*)
        True,

        Echo["Cannot find a model or solution.", "ECMMonPlotSolutions:"];
        $ECMMonFailure
      ];

      If[echoQ,
        Echo[ res, "solutions:" ]
      ];

      ECMMonUnit[res, context]

    ];

ECMMonPlotSolutions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonPlotSolutions[ stockSpec : ( All | _String | {_String ..} | _StringExpression | {_StringExpression..} ), maxTime : (Automatic | _?NumericQ), opts___ ]" <>
            " or ECMMonPlotSolutions[opts___].",
        "ECMMonPlotSolutions:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonPlotSiteSolutions                                    *)
(**************************************************************)

Clear[ECMMonPlotSiteSolutions];

SyntaxInformation[ECMMonPlotSiteSolutions] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonPlotSiteSolutions] =
    Join[
      { "CellIDs" -> None, "Stocks" -> All, "MaxTime" -> 365, "Echo" -> True },
      Options[MultiSiteModelStocksPlot],
      Options[ParametricSolutionsPlots]
    ];

ECMMonPlotSiteSolutions[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonPlotSiteSolutions[xs_, context_Association] := ECMMonAssignInitialConditionsByGridAggregation[][xs, context];

ECMMonPlotSiteSolutions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{cellIDs, stocks, time},

      cellIDs = OptionValue[ECMMonPlotSiteSolutions, "CellIDs"];

      If[ ! MatchQ[ cellIDs, _Integer | {_Integer..} ],
        Echo[
          "The value of the option \"CellIDs\" is expected match the pattern: ( _Integer | {_String..} ).",
          "ECMMonPlotSiteSolutions:"];
        Return[$ECMMonFailure]
      ];

      stocks = OptionValue[ECMMonPlotSiteSolutions, "MaxTime"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _Symbol | {_Symbol..} ) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: ( All | _String | {_String..} | _Symbol | {_Symbol..} ).",
          "ECMMonPlotSiteSolutions:"];
        Return[$ECMMonFailure]
      ];

      time = OptionValue[ECMMonPlotSiteSolutions, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo["The value of the option \"MaxTime\" is expected to be a non-negative number.", "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];

      ECMMonPlotSiteSolutions[ stocks, time, opts][xs, context]
    ];

ECMMonPlotSiteSolutions[
  cellIDs : ( _Integer | { _Integer..} ),
  stocksSpecArg : All | ( _String | {_String..} | _StringExpression),
  maxTime : (Automatic | _?NumericQ),
  opts : OptionsPattern[] ][xs_, context_] :=

    Block[{stocksSpec = Flatten[{stocksSpecArg}], echoQ, stockSymbols, res},

      echoQ = TrueQ[ OptionValue[ECMMonPlotSiteSolutions, "Echo"] ];

      If[ ! ( TrueQ[maxTime === Automatic] || NumericQ[maxTime] && maxTime >= 0 ),
        Echo["The third argument is expected to be Automatic or a non-negative number.", "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];

      Which[

        (*---------*)
        KeyExistsQ[ context, "multiSiteModel"] && KeyExistsQ[ context, "solution"],

        If[TrueQ[ stocksSpec === {All} ],
          stocksSpec = {__ ~~ ___};
        ];

        res = Association @ Map[ # -> SiteIndexSolutionsPlot[ #, context["multiSiteModel"], stocksSpec, context["solution"], maxTime, FilterRules[{opts}, Options[SiteIndexSolutionsPlot]] ]&, cellIDs],

        (*---------*)
        True,

        Echo["Cannot find a multi-site model or solution.", "ECMMonPlotSolutions:"];
        $ECMMonFailure
      ];

      If[echoQ,
        Echo[ res, "solutions:" ]
      ];

      ECMMonUnit[res, context]

    ];

ECMMonPlotSiteSolutions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonPlotSiteSolutions[ stockSpec : All | ( _String | {_String..} | _StringExpression ), maxTime_?NumericQ, opts___ ]" <>
            " or ECMMonPlotSiteSolutions[opts___].",
        "ECMMonPlotSiteSolutions:"
      ];
      $ECMMonFailure
    ];



End[]; (* `Private` *)

EndPackage[]