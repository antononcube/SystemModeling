(*
    System dynamics interactive interfaces functions Mathematica package
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

(* :Title: SystemDynamicsInteractiveInterfacesFunctions *)
(* :Context: SystemDynamicsInteractiveInterfacesFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-06 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["SystemDynamicsInteractiveInterfacesFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

ParametricSolutionsPlots::usage = "ParametricSolutionsPlots[aStocks_Association, aSol_Association, params : (_List | None), tmax_?NumericQ, opts : OptionsPattern[]] \
uses Plot over an association of parametrized functions aSol for the stocks aStocks with function parameters params for time range {0, tmax}.";

ParametricSolutionsListPlots::usage = "ParametricSolutionsListPlots[aStocks_Association, aSol_Association, params : (_List | None), tmax_?NumericQ, opts : OptionsPattern[]] \
uses ListPlot or DateListPlot over an association of parametrized functions aSol for the stocks aStocks with function parameters params for time range {0, tmax}.";

ParametricFunctionValues::usage = "ParametricFunctionValues[pf_ParametricFunction, pars_?AssociationQ, tspec : {tmin_?NumericQ, tmax_?NumericQ, tstep_?NumericQ}] \
evaluates the parametric function pf with parameters pars over the times specified by tspec.";

TrapezoidalRule::usage = "TrapezoidalRule[pnts : {{_?NumericQ, _?NumericQ}..} ] applies the trapezoidal integration rule to list of points.";

TrapezoidalRuleAccumulate::usage = "TrapezoidalRuleAccumulate[pnts : {{_?NumericQ, _?NumericQ}..} ] \
gives accumulated integrals over a list of points.";

StockVariabilityPlot::usage = "StockVariabilityPlot[aSol_Association, stock_Symbol, aPars_Association, {fparVar_Symbol, fparVals_?VectorQ}, tspec : {tmin_?NumericQ, tmax_?NumericQ, tspep_?NumericQ}, opts___]\
makes a plot with different curves with respect to a specified parameters aPars, focus parameter values fparVals over a time grid specification tspec.";

Begin["`Private`"];

(**************************************************************)
(* ParametricSolutionsPlots                                   *)
(**************************************************************)

Clear[ParametricSolutionsPlots];

Options[ParametricSolutionsPlots] =
    Join[{"LogPlot" -> False, "Together" -> False, "Derivatives" -> False, "DerivativePrefix" -> "\[CapitalDelta]"}, Options[Plot]];

ParametricSolutionsPlots[
  aStocks_Association,
  aSol_Association,
  params : (_List | None),
  tmaxArg : (Automatic | _?NumericQ),
  opts : OptionsPattern[]] :=
    Block[{tmax = tmaxArg, logPlotQ, togetherQ, derivativesQ, derivativesPrefix, plotFunc = Plot, dfunc = Identity, dprefix = "", stockRules},

      logPlotQ = TrueQ[OptionValue[ParametricSolutionsPlots, "LogPlot"]];
      togetherQ = TrueQ[OptionValue[ParametricSolutionsPlots, "Together"]];
      derivativesQ = TrueQ[OptionValue[ParametricSolutionsPlots, "Derivatives"]];
      derivativesPrefix = OptionValue[ParametricSolutionsPlots, "DerivativePrefix"];

      stockRules = Normal[aStocks];
      stockRules[[All, 1]] = stockRules[[All, 1]] /. {x_Symbol[id_][v_Symbol] :> x[id]["t"], x_Symbol[v_Symbol] :> x["t"]};

      If[logPlotQ, plotFunc = LogPlot];
      If[derivativesQ, dfunc = D[#, t]&; dprefix = derivativesPrefix];

      If[ TrueQ[tmax === Automatic],
        tmax = Max[Cases[aSol, _InterpolatingFunction, Infinity][[1]]["Domain"]]
      ];

      If[togetherQ,
        List@plotFunc[
          Evaluate[
            If[ Length[params] == 0 || TrueQ[params === None],
              Map[dfunc[#[t]] &, Values[aSol]],
              Map[dfunc[#[Sequence @@ params][t]] &, Values[aSol]]
            ]
          ],
          {t, 0, tmax},
          PlotLegends -> Map[ If[ Length[aStocks] == 0, Row[{dprefix, #1["t"]}], Row[{dprefix, #1["t"], ",", Spacer[3], dprefix, #1["t"] /. stockRules}] ] &, Keys[aSol]],
          Evaluate[FilterRules[Flatten[{opts}], Options[Plot]]],
          PlotRange -> All
        ],
        (*ELSE*)
        KeyValueMap[
          plotFunc[#2, {t, 0, tmax},
            PlotLabel -> If[ Length[aStocks] == 0, Row[{dprefix, #1["t"]}], Row[{dprefix, #1["t"], ",", Spacer[3], dprefix, #1["t"] /. stockRules}] ],
            Evaluate[FilterRules[Flatten[{opts}], Options[Plot]]],
            PlotRange -> All] &,
          If[ Length[params] == 0 || TrueQ[params === None],
            Map[dfunc[#[t]] &, aSol],
            Map[dfunc[#[Sequence @@ params][t]] &, aSol]
          ]
        ]
      ]

    ];


(**************************************************************)
(* ParametricSolutionsListPlots                               *)
(**************************************************************)

Clear[ParametricSolutionsListPlots];

Options[ParametricSolutionsListPlots] =
    Join[
      { "LogPlot" -> False, "Together" -> False, "Derivatives" -> False, "DerivativePrefix" -> "\[CapitalDelta]" },
      { "DateListPlot" -> False, "StartDate" -> Automatic, Joined -> True },
      Union @ Join[ Options[ListPlot], Options[DateListPlot] ]
    ];

ParametricSolutionsListPlots[
  aStocks_Association,
  aSol_Association,
  params : (_List | None),
  tmaxArg : (Automatic | _?NumericQ),
  opts : OptionsPattern[]] :=
    Block[{ tmax = tmaxArg, logPlotQ, togetherQ, derivativesQ, derivativesPrefix,
      dateListPlotQ, startDate, listPlotFuncOpts,
      listPlotFunc = ListPlot, dfunc = Identity, dprefix = "", stockRules,
      ts, aSolCurves },

      logPlotQ = TrueQ[OptionValue[ParametricSolutionsListPlots, "LogPlot"]];
      togetherQ = TrueQ[OptionValue[ParametricSolutionsListPlots, "Together"]];
      derivativesQ = TrueQ[OptionValue[ParametricSolutionsListPlots, "Derivatives"]];
      derivativesPrefix = OptionValue[ParametricSolutionsListPlots, "DerivativePrefix"];

      dateListPlotQ = TrueQ[OptionValue[ParametricSolutionsListPlots, "DateListPlot"]];

      startDate = OptionValue[ParametricSolutionsListPlots, "StartDate"];
      If[ TrueQ[startDate === Automatic], startDate = AbsoluteTime @ Take[Date[], 3] ];

      Which[
        logPlotQ && dateListPlotQ, listPlotFunc = DateListLogPlot,

        !logPlotQ && dateListPlotQ, listPlotFunc = DateListPlot,

        logPlotQ, listPlotFunc = LogPlot
      ];

      listPlotFuncOpts = FilterRules[{opts}, Options[listPlotFunc]];

      stockRules = Normal[aStocks];
      stockRules[[All, 1]] = stockRules[[All, 1]] /. {x_Symbol[id_][v_Symbol] :> x[id]["t"], x_Symbol[v_Symbol] :> x["t"]};

      If[derivativesQ, dfunc = Differences[#]&; dprefix = derivativesPrefix];

      If[ TrueQ[tmax === Automatic],
        tmax = Max[Cases[aSol, _InterpolatingFunction, Infinity][[1]]["Domain"]]
      ];

      ts = Range[0, tmax, 1];

      aSolCurves =
          Map[
            If[ Length[params] == 0 || TrueQ[params === None],
              dfunc[#[ts]],
              dfunc[#[Sequence @@ params][ts]]
            ]&,
            aSol
          ];

      If[ dateListPlotQ,
        aSolCurves = Map[Transpose[{DatePlus[startDate, #] & /@ Range[0, Length[#] - 1], #}] &, aSolCurves]
      ];

      If[togetherQ,
        List @ listPlotFunc[
          Association @ KeyValueMap[#1 -> Tooltip[#2, #1, FilterRules[{opts}, Options[Tooltip]] ] &, aSolCurves],
          PlotLegends -> Map[ If[ Length[aStocks] == 0, Row[{dprefix, #1["t"]}], Row[{dprefix, #1["t"], ",", Spacer[3], dprefix, #1["t"] /. stockRules}] ] &, Keys[aSolCurves]],
          Evaluate[FilterRules[Flatten[{opts}], Options[listPlotFunc]]],
          PlotRange -> All
        ],
        (*ELSE*)
        KeyValueMap[
          listPlotFunc[#2,
            PlotLabel -> If[ Length[aStocks] == 0, Row[{dprefix, #1["t"]}], Row[{dprefix, #1["t"], ",", Spacer[3], dprefix, #1["t"] /. stockRules}] ],
            Evaluate[FilterRules[Flatten[{opts}], Options[listPlotFunc]]],
            PlotRange -> All] &,
          aSolCurves
        ]
      ]
    ];


(**************************************************************)
(* ParametricFunctionValues                                   *)
(**************************************************************)

Clear[ParametricFunctionValues];
ParametricFunctionValues[pfunc_ParametricFunction, aParams_?AssociationQ, tmax_?NumericQ] :=
    ParametricFunctionValues[pfunc, aParams, {0, tmax, 1}];

ParametricFunctionValues[pfunc_ParametricFunction, aParams_?AssociationQ, tspec : {tmin_?NumericQ, tmax_?NumericQ, tstep_?NumericQ}] :=
    Block[{params, ts = Range @@ tspec, f},
      params = pfunc["Parameters"] //. aParams;
      f = pfunc[Sequence @@ params];
      Transpose[{ts, f[ts]}]
    ];


(**************************************************************)
(* TrapezoidalRule                                            *)
(**************************************************************)

Clear[TrapezoidalRule];
TrapezoidalRule[points : {{_?NumericQ, _?NumericQ} ..}, aggrFunc_ : Total] :=
    aggrFunc[Partition[Sort@points, 2, 1] /. {{x1_, y1_}, {x2_, y2_}} :> (x2 - x1) (y1 + (y2 - y1) / 2)];


(**************************************************************)
(* TrapezoidalRuleAccumulate                                  *)
(**************************************************************)

Clear[TrapezoidalRuleAccumulate];
TrapezoidalRuleAccumulate[points : {{_?NumericQ, _?NumericQ} ..}] :=
    Transpose[{Rest@points[[All, 1]], TrapezoidalRule[points, Accumulate]}];


(**************************************************************)
(* StockVariabilityPlot                                  *)
(**************************************************************)

Clear[StockVariabilityPlot];

Options[StockVariabilityPlot] = Join[{"Operation" -> "Identity"}, Options[ListLinePlot]];

StockVariabilityPlot::"nop" = "The value of the option \"Operation\" is expected to be one of `1`";

StockVariabilityPlot[aSol_Association, stock_Symbol,
  aParams_Association, {parVar_Symbol, parRange_?VectorQ},
  tmax_?NumericQ, opts : OptionsPattern[]] :=
    StockVariabilityPlot[aSol, stock, aParams, {parVar, parRange}, {0, tmax, 1}, opts];

StockVariabilityPlot[aSol_Association, stock_Symbol,
  aParams_Association, {parVar_Symbol, parRange_?VectorQ},
  tspec : {tmin_?NumericQ, tmax_?NumericQ, tspep_?NumericQ},
  optsArg : OptionsPattern[]] :=

    Block[{aVals, plots, prefix, funcPaths, funcPathEnds, expectedOperations, operation, opts},

      operation = OptionValue[StockVariabilityPlot, "Operation"];

      opts = FilterRules[{optsArg}, Options[ListLinePlot]];

      aVals = Association@Flatten@Table[v -> ParametricFunctionValues[aSol[stock], Append[aParams, parVar -> v], tspec], {v, parRange}];

      expectedOperations = {"Identity", "Derivative", "Integral"};

      Which[
        operation == "Identity",
        prefix = Nothing;
        funcPaths = Values[aVals];
        plots = ListLinePlot[funcPaths, Evaluate[opts], PlotLegends -> Keys[aVals], ImageSize -> Medium],

        operation == "Derivative",
        prefix = "\[CapitalDelta]";
        funcPaths = Differences[TimeSeries[#]]["Path"] & /@ Values[aVals];
        plots = ListLinePlot[funcPaths, Evaluate[opts], PlotLegends -> Keys[aVals], ImageSize -> Medium],

        operation == "Integral",
        prefix = "\[Integral]";
        funcPaths = TrapezoidalRuleAccumulate /@ Values[aVals];
        plots = ListLinePlot[funcPaths, Evaluate[opts], PlotLegends -> Keys[aVals], ImageSize -> Medium],

        True,
        Message[StockVariabilityPlot::"nop", expectedOperations];
        Return[$Failed]
      ];

      funcPathEnds = funcPaths[[All, -1, 2]];

      Labeled[
        ResourceFunction["GridTableForm"][
          <|
            Row[{prefix, stock}] -> plots,
            Row[{"Differences at ", tspec[[2]]}] ->
                ResourceFunction["GridTableForm"][
                  List @@@ Transpose[{parRange, Round[funcPathEnds, 0.01], Round[(First[#] - # &)@funcPathEnds, 0.01]}],
                  TableHeadings -> {"Parameter", "End values", "Difference\nwith the first"}, Alignment -> "."]
          |>,
          Background -> White],
        Row[{stock, Spacer[5], "wrt", Spacer[5], parVar}], Top
      ]
    ];

End[]; (* `Private` *)

EndPackage[]