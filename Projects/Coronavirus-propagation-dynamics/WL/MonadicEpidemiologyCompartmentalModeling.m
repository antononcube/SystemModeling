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

If[Length[DownValues[SSparseMatrix`ToSSparseMatrix]] == 0,
  Echo["SSparseMatrix.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
];

If[Length[DownValues[HextileBins`HextileBins]] == 0,
  Echo["HextileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HextileBins.m"]
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

$ECMMonFailure::usage = "Failure symbol for the monad ECMMon.";

ECMMonMakeHexagonalGrid::usage = "ECMMonMakeHexagonalGrid";

ECMMonPlotGrid::usage = "ECMMonPlotGrid";

ECMMonExtendByGrid::usage = "ECMMonExtendByGrid";

ECMMonAssignMultiSiteInitialConditions::usage = "ECMMonAssignMultiSiteInitialConditions";

ECMMonSimulate::usage = "ECMMonSimulate";

ECMMonPlotSolutions::usage = "ECMMonPlotSolutions";

Begin["`Private`"];

Needs["StateMonadCodeGenerator`"];
Needs["SSparseMatrix`"];
Needs["HextileBins`"];
Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];
Needs["EpidemiologyModelingSimulationFunctions`"];
Needs["EpidemiologyModelingVisualizationFunctions`"];
Needs["SystemDynamicsInteractiveInterfacesFunctions`"];


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
  {"singleSiteModel", "multiSiteModel", "grid", "solution" },
  "FailureSymbol" -> $ECMMonFailure ];

(**************************************************************)
(* ECMMonSetInitialConditions                                 *)
(**************************************************************)


(**************************************************************)
(* ECMMonSetRates                                             *)
(**************************************************************)


(**************************************************************)
(* ECMMonExtendByGraph                                        *)
(**************************************************************)


(**************************************************************)
(* ECMMonExtendByAdjacencyMatrix                              *)
(**************************************************************)


(**************************************************************)
(* ECMMonMakeHexagonalGrid                                    *)
(**************************************************************)

Clear[ECMMonMakeHexagonalGrid];

SyntaxInformation[ECMMonMakeHexagonalGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[ECMMonMakeHexagonalGrid] = { "Coordinates" -> None, "Radius" -> None };

ECMMonMakeHexagonalGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonMakeHexagonalGrid[xs_, context_Association] := ECMMonMakeHexagonalGrid[][xs, context];

ECMMonMakeHexagonalGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{coords, radius},

      coords = OptionValue[ ECMMonMakeHexagonalGrid, "Coordinates" ];

      If[ ! MatchQ[coords, { {_?NumericQ, _?NumericQ} .. }],
        Echo[
          "The value of the option \"Coordinates\" is expected to be a list of numeric pairs.",
          "ECMMonMakeHexagonalGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      radius = OptionValue[ ECMMonMakeHexagonalGrid, "Radius" ];

      If[ ! ( NumericQ[radius] && radius > 0 ),
        Echo[
          "The value of the option \"Radius\" is expected to be a positive number.",
          "ECMMonMakeHexagonalGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      ECMMonMakeHexagonalGrid[ coords, radius ][xs, context]
    ];

ECMMonMakeHexagonalGrid[coords : { { _?NumericQ, _?NumericQ } .. }, radius_?NumericQ ][xs_, context_] :=
    Block[{aGrid},

      aGrid = MakeHexGrid[coords, radius];

      ECMMonUnit[aGrid, Join[context, <| "grid" -> aGrid |>]]
    ];

ECMMonMakeHexagonalGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonMakeHexagonalGrid[coordinates: { {_?NumericQ, _?NumericQ} .. }, radius_?NumericQ ]"
            <> " or ECMMonMakeHexagonalGrid[OptionsPattern[]].",
        "ECMMonMakeHexagonalGrid:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonPlotGrid                                             *)
(**************************************************************)

Clear[ECMMonPlotGrid];

SyntaxInformation[ECMMonPlotGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[ECMMonPlotGrid] = Join[ {"Echo" -> True}, Options[Graphics] ];

ECMMonPlotGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonPlotGrid[xs_, context_Association] := ECMMonPlotGrid[][xs, context];

ECMMonPlotGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{echoQ, gr},

      echoQ = TrueQ[ OptionValue[ECMMonPlotGrid, "Echo"] ];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found.", "ECMMonPlotGrid:"];
        Return[$ECMMonFailure]
      ];

      gr =
          Graphics[
            {FaceForm[LightBlue], EdgeForm[Red], Values[Map[#Cell &, context["grid"]["Cells"]]]}, opts,
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
(* ECMMonExtendByGrid                                         *)
(**************************************************************)

Clear[ECMMonExtendByGrid];

SyntaxInformation[ECMMonExtendByGrid] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[ECMMonExtendByGrid] = { "Grid" -> None, "Populations" -> None, "Factor" -> 1 };

ECMMonExtendByGrid[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonExtendByGrid[xs_, context_Association] := ECMMonExtendByGrid[][xs, context];

ECMMonExtendByGrid[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{grid, populations, factor},

      grid = OptionValue[ ECMMonMakeHexagonalGrid, "Grid" ];

      If[ ! GridObjectQ[grid],
        Echo[
          "The value of the option \"Grid\" is expected to be a grid object. (See GridObjectQ.)",
          "ECMMonExtendByGrid:"
        ];
        Return[$ECMMonFailure]
      ];

      grid = OptionValue[ ECMMonMakeHexagonalGrid, "Populations" ];

      If[ ! MatchQ[populations, <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |> ],
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

      ECMMonUnit[modelMultiSite, Join[context, <| "multiSiteModel" -> modelMultiSite |>]]

    ] /; MatchQ[aPopulations, <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |> ];

ECMMonExtendByGrid[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonExtendByGrid[ grid_?GridObjectQ, populations: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, factor_?NumericQ ]"
            <> " or ECMMonExtendByGrid[OptionsPattern[]].",
        "ECMMonExtendByGrid:"];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonMakeTravelingPatternsMatrix                          *)
(**************************************************************)


(**************************************************************)
(* ECMMonAssignMultiSiteInitialConditions                       *)
(**************************************************************)

Clear[ECMMonAssignMultiSiteInitialConditions];

SyntaxInformation[ECMMonAssignMultiSiteInitialConditions] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[ECMMonAssignMultiSiteInitialConditions] = { "Default" -> 0 };

ECMMonAssignMultiSiteInitialConditions[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonAssignMultiSiteInitialConditions[xs_, context_Association] := ECMMonAssignMultiSiteInitialConditions[][xs, context];

ECMMonAssignMultiSiteInitialConditions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{},
      Echo["Not implemented signature.", "ECMMonAssignInitialConditions:"];
      $ECMMonFailure
    ];

ECMMonAssignMultiSiteInitialConditions[ aCoordsToValues_Association, stockName_String, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{default, modelMultiSite, aICValues, stockSymbol},

      default = OptionValue[ECMMonAssignMultiSiteInitialConditions, "Default"];

      If[ ! ( NumericQ[default] && default >= 0 ),
        Echo["The value of the option \"Default\" is expected to be a non-negative number.", "ECMMonAssignMultiSiteInitialConditions:"];
        Return[$ECMMonFailure]
      ];

      If[ !KeyExistsQ[ context, "multiSiteModel"],
        Echo["No multi-site model is found. (Required for this signature.)", "ECMMonAssignMultiSiteInitialConditions:"];
        Return[$ECMMonFailure]
      ];

      modelMultiSite = context["multiSiteModel"];

      If[ !KeyExistsQ[ context, "grid"],
        Echo["No grid object is found. (Required for this signature.)", "ECMMonAssignMultiSiteInitialConditions:"];
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

    ] /; MatchQ[aCoordsToValues, <| ({_?NumericQ, _?NumericQ} -> _?NumericQ)... |> ];

ECMMonAssignMultiSiteInitialConditions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is one of ECMMonAssignMultiSiteInitialConditions[ coordsToValues: <| ({_?NumericQ, _?NumericQ} -> _?NumericQ).. |>, stockName_String, opts ].",
        "ECMMonAssignMultiSiteInitialConditions:"
      ];
      $ECMMonFailure
    ];


(**************************************************************)
(* ECMMonSimulate                                             *)
(**************************************************************)

Clear[ECMMonSimulate];

SyntaxInformation[ECMMonSimulate] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonSimulate] = Join[ { "MaxTime" -> 365 }, Options[NDSolve] ];

ECMMonSimulate[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonSimulate[xs_, context_Association] := ECMMonAssignMultiSiteInitialConditions[][xs, context];

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
    Block[{aSol},

      If[ ! ( NumericQ[maxTime] && maxTime >= 0 ),
        Echo["The first argument is expected to be a non-negative number.", "ECMMonSimulate:"];
        Return[$ECMMonFailure]
      ];

      Which[

        KeyExistsQ[ context, "multiSiteModel"],
        aSol = Association @ First @ ModelNDSolve[ context["multiSiteModel"], {Global`t, 0, maxTime}, FilterRules[{opts}, Options[NDSolve]] ],

        KeyExistsQ[ context, "singleSiteModel"],
        aSol = Association @ First @ ModelNDSolve[ context["singleSiteModel"], {Global`t, 0, maxTime}, FilterRules[{opts}, Options[NDSolve]] ],

        True,
        Echo["Cannot find a model.", "ECMMonSimulate:"];
        $ECMMonFailure
      ];


      ECMMonUnit[aSol, Join[ context, <| "solution" -> aSol |>]]

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
(* ECMMonPlotSolutions                                        *)
(**************************************************************)


ParametricSolutionsPlots[]
Clear[ECMMonPlotSolutions];

SyntaxInformation[ECMMonPlotSolutions] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ECMMonPlotSolutions] = Join[ { "Stocks" -> All, "MaxTime" -> 365, "Echo" -> True }, Options[ParametricSolutionsPlots] ];

ECMMonPlotSolutions[___][$ECMMonFailure] := $ECMMonFailure;

ECMMonPlotSolutions[xs_, context_Association] := ECMMonAssignMultiSiteInitialConditions[][xs, context];

ECMMonPlotSolutions[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocks, time},

      stocks = OptionValue[ECMMonPlotSolutions, "MaxTime"];

      If[ ! MatchQ[ stocks, All | ( _String | {_String..} | _Symbol | {_Symbol..} ) ],
        Echo[
          "The value of the option \"Stocks\" is expected match the pattern: All | ( _String | {_String..} | _Symbol | {_Symbol..} ).",
          "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];

      time = OptionValue[ECMMonPlotSolutions, "MaxTime"];

      If[ ! ( NumericQ[time] && time >= 0 ),
        Echo["The value of the option \"MaxTime\" is expected to be a non-negative number.", "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];

      ECMMonPlotSolutions[ stocks, time, opts][xs, context]
    ];

ECMMonPlotSolutions[ stocksSpecArg : All | ( _String | {_String..} | _StringExpression), maxTime_?NumericQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{stocksSpec = Flatten[{stocksSpecArg}], echoQ, res, stockSymbols, aStocks},

      echoQ = TrueQ[ OptionValue[ECMMonPlotSolutions, "Echo"] ];

      If[ ! ( NumericQ[maxTime] && maxTime >= 0 ),
        Echo["The first argument is expected to be a non-negative number.", "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure]
      ];

      Which[

        KeyExistsQ[ context, "multiSiteModel"] && KeyExistsQ[ context, "solution"],
        Echo["Multi-site solutions plot not implemented.", "ECMMonPlotSolutions:"];
        Return[$ECMMonFailure],

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
              "Together" ->True, ImageSize -> Medium, PlotTheme -> "Detailed"
            ],

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
        "The expected signature is one of ECMMonPlotSolutions[ stockSpec : All | ( _String | {_String..} | _StringExpression ), maxTime_?NumericQ, opts___ ].",
        "ECMMonPlotSolutions:"
      ];
      $ECMMonFailure
    ];

(**************************************************************)
(* ECMMonPlotGraphNodesWithSolutionValues                     *)
(**************************************************************)


End[]; (* `Private` *)

EndPackage[]