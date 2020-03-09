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

ParametricSolutionsPlots::usage = "Uses Plot over an association of parametrized functions.";

Begin["`Private`"];

Clear[ParametricSolutionsPlots];

Options[ParametricSolutionsPlots] =
    Join[{"LogPlot" -> False, "Together" -> False, "Derivatives" -> False, "DerivativePrefix" -> "\[CapitalDelta]"}, Options[Plot]];

ParametricSolutionsPlots[aStocks_Association, aSol_Association, params_List, tmax_?NumberQ, opts : OptionsPattern[]] :=
    Block[{logPlotQ, togetherQ, derivativesQ, derivativesPrefix, plotFunc = Plot, dfunc = Identity, dprefix = "", stockRules},

      logPlotQ = TrueQ[OptionValue[ParametricSolutionsPlots, "LogPlot"]];
      togetherQ = TrueQ[OptionValue[ParametricSolutionsPlots, "Together"]];
      derivativesQ = TrueQ[OptionValue[ParametricSolutionsPlots, "Derivatives"]];
      derivativesPrefix = OptionValue[ParametricSolutionsPlots, "DerivativePrefix"];

      stockRules = Normal[aStocks];
      stockRules[[All,1]] = stockRules[[All,1]] /. x_Symbol[v_Symbol] :> x["t"];

      If[logPlotQ, plotFunc = LogPlot];
      If[derivativesQ, dfunc = D[#, t]&; dprefix = derivativesPrefix];

      If[togetherQ,
        List@plotFunc[
          Evaluate[Map[dfunc[#[Sequence @@ params][t]] &, Values[aSol]]], {t, 0, tmax},
          PlotLegends -> Map[Row[{dprefix, #1["t"], ",", Spacer[3], dprefix, #1["t"] /. stockRules}] &, Keys[aSol]],
          Evaluate[FilterRules[Flatten[{opts}], Options[Plot]]]
        ],
        (*ELSE*)
        KeyValueMap[
          plotFunc[#2, {t, 0, tmax},
            PlotLabel -> Row[{dprefix, #1["t"], ",", Spacer[3], dprefix, #1["t"] /. stockRules}],
            Evaluate[FilterRules[Flatten[{opts}], Options[Plot]]]] &,
          Map[dfunc[#[Sequence @@ params][t]] &, aSol]]
      ]
      
    ];

End[]; (* `Private` *)

EndPackage[]