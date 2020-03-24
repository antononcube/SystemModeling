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


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)


BeginPackage["EpidemiologyModelingSimulationFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

ModelNDSolve::usage = "ModelNDSolve[model, {t, maxTime}, opts] simulates the model from 0 to maxTime using NDSolve";

Begin["`Private`"];

Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];


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

      lsActualEquations =
          Join[
            model["Equations"] //. Join[Association[ Rule @@@ model["InitialConditions"] ], model["RateRules"]],
            model["InitialConditions"] //. model["RateRules"]
          ];

      NDSolve[lsActualEquations, GetStockSymbols[model], {var, 0, maxTime}, FilterRules[{opts}, NDSolve] ]

    ] /; TrueQ[ Head[var] === Symbol] && maxTime > 0;

ModelNDSolve[___] :=
    Block[{},
      Message[ModelNDSolve::"nargs"];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]