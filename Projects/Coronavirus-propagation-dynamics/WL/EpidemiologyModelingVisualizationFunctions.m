(*
    Epidemiology modeling visualization functions Mathematica package
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

(* :Title: EpidemiologyModelingVisualizationFunctions *)
(* :Context: EpidemiologyModelingVisualizationFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)


(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[EpidemiologyModelModifications`GetStockSymbols]] == 0,
  Echo["EpidemiologyModelModifications.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelModifications.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["EpidemiologyModelingVisualizationFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

EvaluateSolutionsByModelIDs::usage = "EvaluateSolutionsOverModelIDs[mdl, sns, aSol, trng] \
evaluates the solutions in aSol for the stock names sns for each ID in the model mdl over the specified time range trng.";

EvaluateSolutionsOverGraphVertexes::usage = "EvaluateSolutionsOverGraphVertexes[gr, mdl, sns, aSol, trng] \
evaluates the solutions in aSol for the stock names sns for each (vertex) ID in the model mdl over the specified time range trng. \
(Legacy function.)";

EvaluateSolutionsOverGraph::usage = "EvaluateSolutionsOverGraph[gr, model, stockNames, aSol, timeRange, opts] \
makes a sequence of graph plots of the graph gr with the graph nodes colored according solution functions aSol.";

MakeVertexShapeFunction::usage = "MakeVertexShapeFunction makes a vertex shape function.";

ConvertSolutions::usage = "ConvertSolutions[ aSolEvals, type] converts an association of solution evaluations into
a dataset.";

PopulationStockPlots::usage = "PopulationStockPlots[grHexagonCells_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_Association, stocksArg : (_String | {_String ..}), maxTime_?NumberQ, addOpts : OptionsPattern[]]";

EconomicsStockPlots::usage = "EconomicsStockPlots[grHexagonCells_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_Association, stock_String, maxTime_?NumberQ, opts : OptionsPattern[] ]";


Begin["`Private`"];

Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];

(**************************************************************)
(* EvaluateSolutionsByModelIDs                         *)
(**************************************************************)

Clear[EvaluateSolutionsByModelIDs];

EvaluateSolutionsByModelIDs::"ntr" = "The fifth argument is expected to be a valid time range specification.";

EvaluateSolutionsByModelIDs::"nst" = "No model stocks are found with the specification given as the third argument.";

EvaluateSolutionsByModelIDs[
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  maxTimeArg : (Automatic | _?NumberQ) ] :=
    EvaluateSolutionsByModelIDs[ model, stockNames, aSol, {1, maxTimeArg, 1}];

EvaluateSolutionsByModelIDs[
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  {minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ)} ] :=
    EvaluateSolutionsByModelIDs[ model, stockNames, aSol, {minTime, maxTimeArg, 1}];

EvaluateSolutionsByModelIDs[
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  { minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ), step_?NumberQ } ] :=

    Block[{maxTime = maxTimeArg, stockSymbols, stockValues},

      If[TrueQ[maxTime === Automatic],
        (* Assuming all solution functions have the same domain. *)
        maxTime = Max[Flatten[aSol[[1]]["Domain"]]]
      ];

      If[ step == 0 || (step > 0 && minTime > maxTime) || (step < 0 && minTime < maxTime),
        Message[EvaluateSolutionsOverGraph::"ntr"];
        Return[$Failed]
      ];

      stockSymbols = Union @ Flatten @ Map[ Cases[GetStockSymbols[model, #], p_[id_] :> p]&, Flatten[{stockNames}] ];

      If[ Length[stockSymbols] == 0,
        Message[EvaluateSolutionsByModelIDs::"nst"];
        Return[$Failed]
      ];

      stockValues = Map[ #[ Range[minTime, maxTime, step] ]&, KeyTake[ aSol, Union @ Flatten @ Map[ GetStockSymbols[model, #]&, Flatten[{stockNames}] ] ] ];

      stockValues = GroupBy[ Normal[stockValues], #[[1, 1]]&, Total @ #[[All, 2]] & ];

      stockValues
    ];

EvaluateSolutionsByModelIDs[gr_Graph, args___] := EvaluateSolutionsByModelIDs[args];


(**************************************************************)
(* EvaluateSolutionsOverGraphVertexes                         *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraphVertexes];

EvaluateSolutionsOverGraphVertexes = EvaluateSolutionsByModelIDs;


(**************************************************************)
(* EvaluateSolutionsOverGraph                                 *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraph];

EvaluateSolutionsOverGraph::"ncs" = "The value of the option \"ColorScheme\" is expected to be a string.";

EvaluateSolutionsOverGraph::"nnsf" = "The value of the option \"NodeSizeFactor\" is expected to be a positive number.";

EvaluateSolutionsOverGraph::"nnorm" = "The value of the option \"Normalization\" is expected to be one of `1`.";

EvaluateSolutionsOverGraph::"ntr" = "The fifth argument is expected to be a valid time range specification.";

EvaluateSolutionsOverGraph::"nst" = "No model stocks are found with the specification given as the third argument.";

Options[EvaluateSolutionsOverGraph] =
    Join[
      {"ColorScheme" -> "TemperatureMap", "NodeSizeFactor" -> 1, "TimePlotLabels" -> True, "Normalization" -> Automatic, "Legended" -> False },
      Options[GraphPlot]
    ];

EvaluateSolutionsOverGraph[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  maxTimeArg : (Automatic | _?NumberQ),
  opts : OptionsPattern[]] :=
    EvaluateSolutionsOverGraph[ gr, model, stockNames, aSol, {1, maxTimeArg, 1}, opts];

EvaluateSolutionsOverGraph[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  {minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ)},
  opts : OptionsPattern[]] :=
    EvaluateSolutionsOverGraph[ gr, model, stockNames, aSol, {minTime, maxTimeArg, 1}, opts];

EvaluateSolutionsOverGraph[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  { minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ), step_?NumberQ },
  opts : OptionsPattern[]] :=

    Block[{cf, nodeSizeFactor, timeLabelsQ, normalization, legendedQ, maxTime = maxTimeArg, expected,
      stockSymbols, vf, stockValues, maxStockValue, res},

      cf = OptionValue[EvaluateSolutionsOverGraph, "ColorScheme"];
      If[! StringQ[cf],
        Message[EvaluateSolutionsOverGraph::"ncs"];
        Return[$Failed]
      ];

      nodeSizeFactor = OptionValue[EvaluateSolutionsOverGraph, "NodeSizeFactor"];
      If[! (NumberQ[nodeSizeFactor] && nodeSizeFactor > 0),
        Message[EvaluateSolutionsOverGraph::"nnsf"];
        Return[$Failed]
      ];

      timeLabelsQ = TrueQ[ OptionValue[EvaluateSolutionsOverGraph, "TimePlotLabels"] ];

      normalization = OptionValue[EvaluateSolutionsOverGraph, "Normalization"];
      expected = {Automatic, "Global", "ByVertex", "byNode"};
      If[ ! MemberQ[ expected, normalization ] ,
        Message[EvaluateSolutionsOverGraph::"nnorm", ToString[InputForm[expected]] ];
        Return[$Failed]
      ];

      legendedQ = TrueQ[ OptionValue[EvaluateSolutionsOverGraph, "Legended"] ];

      If[TrueQ[maxTime === Automatic],
        (* Assuming all solution functions have the same domain. *)
        maxTime = Max[Flatten[aSol[[1]]["Domain"]]]
      ];

      If[ step == 0 || (step > 0 && minTime > maxTime) || (step < 0 && minTime < maxTime),
        Message[EvaluateSolutionsOverGraph::"ntr"];
        Return[$Failed]
      ];

      stockSymbols = Union @ Flatten @ Map[ Cases[GetStockSymbols[model, #], p_[id_] :> p]&, Flatten[{stockNames}] ];

      If[ Length[stockSymbols] == 0,
        Message[EvaluateSolutionsOverGraph::"nst"];
        Return[$Failed]
      ];

      (* Some of the work above is repeated in EvaluateSolutionsByModelIDs. *)
      stockValues = EvaluateSolutionsByModelIDs[ model, stockNames, aSol, {minTime, maxTime, step} ];

      If[ MemberQ[ {Automatic, "Global" }, normalization],

        maxStockValue = Max[Values[stockValues]];

        vf[time_][{xc_, yc_}, name_, {w_, h_}] :=
            {
              ColorData[cf, "ColorFunction"][Total@Rescale[Map[aSol[#[name]][time] &, stockSymbols], {0, maxStockValue}, {0, 1}]],
              Rectangle[{xc - nodeSizeFactor * w, yc - nodeSizeFactor * h}, {xc + nodeSizeFactor * w, yc + nodeSizeFactor * h}]
            },
        (* ELSE *)

        maxStockValue = Max /@ stockValues;

        vf[time_][{xc_, yc_}, name_, {w_, h_}] :=
            {
              ColorData[cf, "ColorFunction"][Total@Rescale[Map[aSol[#[name]][time] &, stockSymbols], {0, maxStockValue[name]}, {0, 1}]],
              Rectangle[{xc - nodeSizeFactor * w, yc - nodeSizeFactor * h}, {xc + nodeSizeFactor * w, yc + nodeSizeFactor * h}]
            }
      ];

      res =
          Table[
            GraphPlot[gr, VertexShapeFunction -> vf[t], FilterRules[ Join[ If[timeLabelsQ, {PlotLabel -> t}, {}], {opts} ], Options[GraphPlot]]],
            {t, Range[minTime, maxTime, step]}
          ];

      Which[
        legendedQ && MemberQ[ {Automatic, "Global" }, normalization],
        Legended[ res, BarLegend[{cf, MinMax[stockValues]}]],

        legendedQ,
        Legended[ res, BarLegend[cf]],

        True,
        res
      ]
    ];


(**************************************************************)
(* MakeVertexShapeFunction                                    *)
(**************************************************************)

Clear[MakeVertexShapeFunction];

SetAttributes[MakeVertexShapeFunction, HoldAll];

MakeVertexShapeFunction[vfName_Symbol, stockArg_Symbol, timeArg_, aSolArg_, maxPopulationArg_, colorScheme_, factorArg_] :=
    With[{vf = vfName, stock = stockArg, aSol = HoldForm[aSolArg],
      time = timeArg, maxPopulation = maxPopulationArg,
      factor = factorArg, cf = colorScheme},
      vf[{xc_, yc_}, name_, {w_, h_}] := {
        ColorData[cf, "ColorFunction"][Rescale[aSol[[1]][stock[name]][time], {0, maxPopulation}, {0, 1}]],
        Rectangle[{xc - factor w, yc - factor h}, {xc + factor w, yc + factor h}]
      };
    ];

MakeVertexShapeFunction[___] := $Failed;


(**************************************************************)
(* ConvertSolutions                                           *)
(**************************************************************)

Clear[ConvertSolutions];

ConvertSolutions::"ntype" = "Unknown conversion type.";

ConvertSolutions[ aStockSolutionValues : Association[(_String -> Association[(_ -> _?VectorQ) ..]) ..], type_String : "Dataset" ] :=
    Block[{res},

      res =
          Join @@
              KeyValueMap[
                Function[{k, v},
                  Join @@ KeyValueMap[ Thread[{k, #1, Range[Length[#2]], #2 }]&, v]
                ],
                aStockSolutionValues
              ];

      Which[
        type == "Dataset",
        Dataset[Dataset[res][All, AssociationThread[#, {"Stock", "Node", "Time", "Value"}]& ]],

        type == "Array",
        res,

        True,
        Message[ConvertSolutions::"ntype"];
        $Failed
      ]

    ];


(**************************************************************)
(* PopulationStockPlots                                       *)
(**************************************************************)

Clear[PopulationStockPlots];

PopulationStockPlots::"nst" = "At least one of the specified stocks is not known.";

Options[PopulationStockPlots] = Options[EvaluateSolutionsByModelIDs];

PopulationStockPlots[grHexagonCellsDummy_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stocksArg : (_String | {_String ..}), maxTime_?NumericQ, opts : OptionsPattern[]] :=
    PopulationStockPlots[modelMultiSite, aSolMultiSite, stocksArg, maxTime, opts];

PopulationStockPlots[modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stocksArg : (_String | {_String ..}), maxTime_?NumericQ, opts : OptionsPattern[]] :=
    Block[{lsLocalOpts = {PlotTheme -> "Detailed", PlotRange -> All, ImageSize -> Medium}, stocks = Flatten[{stocksArg}], vals},

      lsLocalOpts = Join[{opts}, lsLocalOpts];

      If[Apply[And, MemberQ[Union[Values[modelMultiSite["Stocks"]]], #] & /@ stocks],

        vals = Total@Map[Total@Values[EvaluateSolutionsByModelIDs[modelMultiSite, #, aSolMultiSite, {1, maxTime, 1}]] &, stocks];

        Row[{
          ListLinePlot[
            vals,
            lsLocalOpts,
            PlotLabel -> StringRiffle[stocks, " + "]
          ],
          Spacer[10],
          ListLinePlot[
            vals / Total[Values[EvaluateSolutionsByModelIDs[modelMultiSite, "Total Population", aSolMultiSite, {1, maxTime, 1}]]],
            lsLocalOpts,
            PlotLabel -> Row[{"Ratio of", Spacer[2], StringRiffle[stocks, " + "], Spacer[2], "with Total Population"}]
          ]
        }],
        (*ELSE*)
        Message[PopulationStockPlots::"nst"];
        $Failed
      ]
    ];

PopulationStockPlots[___] := $Failed;

(**************************************************************)
(* EconomicsStockPlots                                        *)
(**************************************************************)

Clear[EconomicsStockPlots];

EconomicsStockPlots::"nst" = "The specified stock is not known.";

Options[EconomicsStockPlots] = Options[EvaluateSolutionsByModelIDs];

EconomicsStockPlots[grHexagonCellsDummmy_Graph, modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stock_String, maxTime_?NumericQ, opts : OptionsPattern[] ] :=
    EconomicsStockPlots[modelMultiSite, aSolMultiSite, stock, maxTime, opts];

EconomicsStockPlots[modelMultiSite_?EpidemiologyModelQ, aSolMultiSite_?AssociationQ, stock_String, maxTime_?NumericQ, opts : OptionsPattern[] ] :=
    Block[{lsLocalOpts = {PlotTheme -> "Detailed", PlotRange -> All, ImageSize -> Medium}},

      lsLocalOpts = Join[{opts}, lsLocalOpts];

      If[ MemberQ[Union[Values[modelMultiSite["Stocks"]]], stock],
        Row[{
          ListLinePlot[
            Total[Values[EvaluateSolutionsByModelIDs[modelMultiSite, stock, aSolMultiSite, {1, maxTime, 1}]]],
            lsLocalOpts,
            PlotLabel -> stock
          ],
          Spacer[10],
          ListLinePlot[
            Differences@Total[Values[EvaluateSolutionsByModelIDs[modelMultiSite, stock, aSolMultiSite, {1, maxTime, 1}]]],
            lsLocalOpts,
            PlotLabel -> Row[{"\[CapitalDelta]", Spacer[1], stock}]
          ]
        }],
        (* ELSE *)
        Message[EconomicsStockPlots::"nst"];
        $Failed
      ]

    ];

EconomicsStockPlots[___] := $Failed;

End[]; (* `Private` *)

EndPackage[]