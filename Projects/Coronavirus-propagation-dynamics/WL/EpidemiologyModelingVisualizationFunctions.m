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

EvaluateSolutionsOverGraph::usage = "EvaluateSolutionsOverGraph";

Begin["`Private`"];

Needs["EpidemiologyModelModifications`"];

(**************************************************************)
(* EvaluateSolutionsOverGraph                                 *)
(**************************************************************)

Clear[EvaluateSolutionsOverGraph];

EvaluateSolutionsOverGraph::"ncs" = "The value of the option \"ColorScheme\" is expected to be a string.";

EvaluateSolutionsOverGraph::"nnsf" = "The value of the option \"NodeSizeFactor\" is expected to be a positive number.";

EvaluateSolutionsOverGraph::"nmt" = "The value of the fifth argument (min time) is expected to be smaller or equal \
to the value of the sixth argument (max time).";

Options[EvaluateSolutionsOverGraph] = Join[{"ColorScheme" -> "TemperatureMap", "NodeSizeFactor" -> 1}, Options[GraphPlot]];

EvaluateSolutionsOverGraph[gr_Graph, model_Association, stockNames_ : (_String | {_String ..}), aSol_Association,
  minTime_?NumberQ, maxTimeArg : (Automatic | _?NumberQ),
  opts : OptionsPattern[]] :=

    Block[{cf, nodeSizeFactor, maxTime = maxTimeArg, stockSymbols, vf, maxStockValue},

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

      If[TrueQ[maxTime === Automatic],
        (* Assuming all solution functions have the same domain. *)
        maxTime = Max[Flatten[aSol[[1]]["Domain"]]]
      ];

      If[ minTime > maxTime,
        Message[EvaluateSolutionsOverGraph::"nmt"];
        Return[$Failed]
      ];

      stockSymbols = Union[Cases[GetStockSymbols[model, stockNames], p_[id_] :> p]];

      maxStockValue = Max[Map[#[maxTime] &, aSol]];

      vf[time_][{xc_, yc_}, name_, {w_, h_}] :=
          {
            ColorData[cf, "ColorFunction"][Total@Rescale[Map[aSol[#[name]][time] &, stockSymbols], {0, maxStockValue}, {0, 1}]],
            Rectangle[{xc - nodeSizeFactor * w, yc - nodeSizeFactor * h}, {xc + nodeSizeFactor * w, yc + nodeSizeFactor * h}]
          };

      Table[GraphPlot[gr, VertexShapeFunction -> vf[t],
        FilterRules[{opts}, Options[GraphPlot]]], {t,
        Range[minTime, maxTime, 1]}]
    ];

End[]; (* `Private` *)

EndPackage[]