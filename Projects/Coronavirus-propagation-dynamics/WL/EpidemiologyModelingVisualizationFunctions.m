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

(* :Title: EpidemiologyVisualizationFunctions *)
(* :Context: EpidemiologyVisualizationFunctions` *)
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

BeginPackage["EpidemiologyVisualizationFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

EvaluateSolutionsOverGraphVertexes::usage = "EvaluateSolutionsOverGraphVertexes[gr, model, stockNames, aSol, timeRange] \
evaluates the solutions aSol for each vertex of gr over the specified time range timeRange.";

EvaluateSolutionsOverGraph::usage = "EvaluateSolutionsOverGraph[gr, model, stockNames, aSol, timeRange, opts] \
makes a sequence of graph plots of the graph gr with the graph nodes colored according solution functions aSol.";

MakeVertexShapeFunction::usage = "MakeVertexShapeFunction makes a vertex shape function.";

Begin["`Private`"];

Needs["EpidemiologyModelModifications`"];

(**************************************************************)
(* EvaluateSolutionsOverGraphVertexes                         *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraphVertexes];

EvaluateSolutionsOverGraphVertexes::"ntr" = "The fifth argument is expected to be a valid time range specification.";

EvaluateSolutionsOverGraphVertexes[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  maxTimeArg : (Automatic | _?NumberQ) ] :=
    EvaluateSolutionsOverGraphVertexes[ gr, model, stockNames, aSol, {1, maxTimeArg, 1}];

EvaluateSolutionsOverGraphVertexes[
  gr_Graph,
  model_Association,
  stockNames_ : ( (_String | _StringExpression) | { (_String | _StringExpression) ..} ),
  aSol_Association,
  {minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ)} ] :=
    EvaluateSolutionsOverGraphVertexes[ gr, model, stockNames, aSol, {minTime, maxTimeArg, 1}];

EvaluateSolutionsOverGraphVertexes[
  gr_Graph,
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

      stockValues = Map[ #[ Range[minTime, maxTime, step] ]&, KeyTake[ aSol, Union @ Flatten @ Map[ GetStockSymbols[model, #]&, Flatten[{stockNames}] ] ] ];

      stockValues = GroupBy[ Normal[stockValues], #[[1, 1]]&, Total @ #[[All, 2]] & ];

      stockValues
    ];


(**************************************************************)
(* EvaluateSolutionsOverGraph                                 *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraph];

EvaluateSolutionsOverGraph::"ncs" = "The value of the option \"ColorScheme\" is expected to be a string.";

EvaluateSolutionsOverGraph::"nnsf" = "The value of the option \"NodeSizeFactor\" is expected to be a positive number.";

EvaluateSolutionsOverGraph::"nnorm" = "The value of the option \"Normalization\" is expected to be one of `1`.";

EvaluateSolutionsOverGraph::"ntr" = "The fifth argument is expected to be a valid time range specification.";

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

      stockValues = EvaluateSolutionsOverGraphVertexes[ gr, model, stockNames, aSol, {minTime, maxTime, step} ];

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

      If[ legendedQ,
        Legended[ res, BarLegend[{cf, MinMax[stockValues]}]],
        (*ELSE*)
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
        ColorData[cf, "ColorFunction"][
          Rescale[aSol[[1]][stock[name]][time], {0, maxPopulation}, {0, 1}]],
        Rectangle[{xc - factor w, yc - factor h}, {xc + factor w, yc + factor h}]
      };
    ];

MakeVertexShapeFunction[___] := $Failed;

End[]; (* `Private` *)

EndPackage[]