(*
    System dynamics model graph Mathematica package
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
(* :Title: ModelStockDependencies *)
(* :Context: ModelStockDependencies` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-12-05 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["ModelStockDependencies`"];

ModelStockDependencies::usage = "ModelStockDependencies[ modelEqs : {_Equal ..}, modelStocks_List, tvar_Symbol, opts___] \
finds the dependencies of the stocks modelStocks over the variable tvar through the equations modelEqs.";

ModelGraph::usage = "ModelGraph[ modelEqs : {_Equal ..}, modelStocks_List, tvar_Symbol, opts___] \
makes a graph for the dependencies of the stocks modelStocks over the variable tvar through the equations modelEqs.";

Begin["`Private`"];

(**************************************************************)
(* ModelStockDependencies                                     *)
(**************************************************************)

Clear[ModelStockDependencies];

SyntaxInformation[ModelStockDependencies] = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

ModelStockDependencies::nargs = "The first argument is expected to be a list of equations. \
The second argument is expected to be a list of stocks. \
The third argument is expected to be symbol.";

Options[ModelStockDependencies] = {"ExpandEquations" -> False, "IncludeEquationIndexes" -> Automatic};

ModelStockDependencies[lsModelEquations : {_Equal ..}, lsModelStocks_List, tvar_Symbol, opts : OptionsPattern[]] :=
    Block[{aLHSIndexes, aRHSIndexes, inclEqIndexesQ = False},

      aLHSIndexes =
          Association@
              Map[
                Function[{s},
                  s -> Fold[If[FreeQ[lsModelEquations[[#2, 1]], s], #1, Append[#1, #2], #1] &, {}, Range[Length[lsModelEquations]]]
                ],
                lsModelStocks
              ];

      aRHSIndexes =
          Association@
              Map[
                Function[{s},
                  s -> Fold[If[FreeQ[lsModelEquations[[#2, 2]], s], #1, Append[#1, #2], #1] &, {}, Range[Length[lsModelEquations]]]],
                lsModelStocks
              ];

      inclEqIndexesQ = OptionValue[ModelStockDependencies, "IncludeEquationIndexes"];

      If[TrueQ[inclEqIndexesQ === Automatic],
        inclEqIndexesQ = ! Apply[And, Length[#] == 1 & /@ Values[aLHSIndexes]],
        (*ELSE*)
        inclEqIndexesQ = TrueQ[inclEqIndexesQ]
      ];

      Join @@
          Map[
            FocusStockDependencies[lsModelEquations, lsModelStocks, aLHSIndexes, aRHSIndexes, #, tvar, "IncludeEquationIndexes" -> inclEqIndexesQ, opts] &,
            lsModelStocks
          ]
    ];

ModelStockDependencies[___] :=
    Block[{},
      Message[ModelStockDependencies::nargs];
      $Failed
    ];


(**************************************************************)
(* FocusStockDependencies                                     *)
(**************************************************************)

Clear[FocusStockDependencies];

Options[FocusStockDependencies] = Options[ModelStockDependencies];

FocusStockDependencies[
  lsModelEquations : {_Equal ..},
  lsStocks_List,
  aLHSIndexes_?AssociationQ,
  aRHSIndexes_?AssociationQ,
  focusStock_,
  tvar_Symbol,
  opts : OptionsPattern[]] :=

    Block[{lsStockEquations, aRes, expandFunc = Identity, inclEqIndexesQ},

      If[TrueQ[OptionValue[FocusStockDependencies, "ExpandEquations"]], expandFunc = Expand];
      inclEqIndexesQ = OptionValue[FocusStockDependencies, "IncludeEquationIndexes"];

      lsStockEquations = lsModelEquations[[aLHSIndexes[focusStock], 2]];

      aRes =
          Association@Flatten@
              Map[
                Function[{s},
                  With[{s2 = s[tvar]},
                    {focusStock, s} ->
                        Flatten@MapThread[Thread[#2 -> Cases[expandFunc[#1], (Times[f__, s2] | Times[s2, f__]) :> f, Infinity]] &, {lsStockEquations, aLHSIndexes[focusStock]}]
                  ]
                ],
                lsStocks
              ];

      aRes = Map[Select[#, Length[#[[2]]] > 0 &] &, aRes];
      aRes = Select[aRes, Length[#] > 0 &];

      If[! inclEqIndexesQ,
        aRes = Map[#[[All, 2]] &, aRes]
      ];

      aRes
    ];


(**************************************************************)
(* ModelGraph                                                 *)
(**************************************************************)

Clear[ModelGraph];

SyntaxInformation[ModelGraph] = {"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}};

ModelGraph::nargs = "The first argument is expected to be a list of equations. \
The second argument is expected to be a list of stocks. \
The third argument is expected to be symbol.";

Options[ModelGraph] = Join[ Options[ModelStockDependencies], Options[Graph]];

ModelGraph[lsModelEquations : {_Equal ..}, lsModelStocks_List, tvar_Symbol, opts : OptionsPattern[]] :=
    Block[{lsSSRules},
      lsSSRules =
          Flatten@KeyValueMap[Thread[DirectedEdge[#1[[2]], #[[1]], #2]] &,
            ModelStockDependencies[lsModelEquations, lsModelStocks, tvar, FilterRules[{opts}, Options[ModelStockDependencies]]]
          ];

      Graph[lsSSRules,
        FilterRules[{opts}, Options[Graph]],
        VertexLabels -> "Name", EdgeLabels -> "EdgeTag"]
    ];

ModelGraph[___] :=
    Block[{},
      Message[ModelGraph::nargs];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]