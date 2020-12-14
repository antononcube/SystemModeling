(*
    System Dynamics Model Graph Mathematica package
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
(* :Title: SystemDynamicsModelGraph *)
(* :Context: SystemDynamicsModelGraph` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-12-05 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: dependency graph, call graph, ODE, equations system, System Dynamics *)
(* :Discussion:

   # In brief

   This package provides functions for making dependency graphs for the stocks in System Dynamics models.
   (I.e. the variable dependent functions in systems of ODEs.)

   # Usage example

   Here is list of (epidemiology compartmental model) equations:

   ```mathematica
   lsEqs = {
     Derivative[1][SP][t] == -((IP[t] SP[t] \[Beta][IP])/TP[t]) - SP[t] \[Mu][TP],
     Derivative[1][EP][t] == (IP[t] SP[t] \[Beta][IP])/TP[t] - EP[t] (1/aincp + \[Mu][TP]),
     Derivative[1][IP][t] == EP[t]/aincp - IP[t]/aip - IP[t] \[Mu][IP],
     Derivative[1][RP][t] == IP[t]/aip - RP[t] \[Mu][TP],
     TP[t] == Max[0, EP[t] + IP[t] + RP[t] + SP[t]]}
   ```

   Here is a graph of the dependencies between the populations:

   ```mathematica
   ModelDependencyGraph[lsEqs, {SP, EP, IP, RP, TP}, t]
   ```

   The function `ModelDependencyGraph` takes all options of `Graph`:

   ```mathematica
   ModelDependencyGraph[lsEqs, {SP, EP, IP, RP, TP}, t,
     GraphLayout -> "GravityEmbedding", VertexLabels -> "Name",
     VertexLabelStyle -> Directive[Red, Bold, 16], EdgeLabelStyle -> Directive[Blue, 16]]
   ```

   Here is graph for "focus" stocks-sources to "focus" stocks-destinations:

   ```mathematica
   ModelDependencyGraph[lsEqs, {IP, SP}, {EP}, t]
   ```

   Compare with:

   ```mathematica
   ModelDependencyGraph[lsEqs, {EP}, {IP, SP}, t]
   ```

   # Dependencies only

   The dependencies in the model can be found with the function ModelDependencyGraphEdges:

   ```mathematica
   ModelDependencyGraphEdges[lsEqs, Automatic, t]
   ```

   # Interfacing

   This functions of this package works with the models from the package [AAp1].

   Here is a model from [AAp1]:

   ```mathematica
   model = SEIRModel[t, "TotalPopulationRepresentation" -> "AlgebraicEquation"]
   ```

   Here we find the stocks dependencies:

   ```mathematica
   SystemDynamicsModelGraph[model, t]
   ```

   Here we make the corresponding graph:

   ```
   ModelDependencyGraph[model, t]
   ```

   # References

   [AAp1] Anton Antonov, "Epidemiology models Mathematica package", (2020), SystemsModeling at GitHub/antononcube.
          https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m


TODO:
  1. [X] Usage examples
  2. [X] Automatic stocks
  3. [ ] Make unit tests
  4. [ ] Adding tooltips
  5. [ ] Functions to deal with large models.
  6. [ ] Describe the reasons to use or not use Simplify, Expand, Gather, etc.
  7. [ ] Make a system of ODEs from graph edges.

*)

BeginPackage["SystemDynamicsModelGraph`"];

ModelHeuristicStocks::usage = "ModelHeuristicStocks[modelEqs : {_Equal ..}] \
find heuristically the stocks in a list of equations.";

SystemDynamicsModelGraph::usage = "SystemDynamicsModelGraph[ modelEqs : {_Equal ..}, modelStocks_List, tvar_Symbol, opts___] \
finds the dependencies of the stocks modelStocks over the variable tvar through the equations modelEqs.";

ModelDependencyGraphEdges::usage = "ModelDependencyGraphEdges[ modelEqs : {_Equal ..}, modelStocks_List, tvar_Symbol, opts___] \
makes dependency rules of the stocks modelStocks over the variable tvar through the equations modelEqs.";

ModelDependencyGraph::usage = "ModelDependencyGraph[ modelEqs : {_Equal ..}, modelStocks_List, tvar_Symbol, opts___] \
makes a graph for the dependencies of the stocks modelStocks over the variable tvar through the equations modelEqs.";

ModelGraphEquations::usage = "ModelGraphEquations[ edges_ : { DirectedEdge[_, _, _].. }, tvar_Symbol ] \
generates model equations for a list directed edges.";

Begin["`Private`"];

(***********************************************************)
(* EpidemiologyModelQ                                      *)
(***********************************************************)

Clear[EpidemiologyModelQ];
EpidemiologyModelQ[model_] :=
    AssociationQ[model] &&
        Length[ Intersection[ Keys[model], { "Stocks", "Rates", "Equations" } ] ] == 3 &&
        AssociationQ[model["Stocks"]] &&
        AssociationQ[model["Rates"]] &&
        MatchQ[model["Equations"], { _Equal .. }];


(**************************************************************)
(* ModelHeuristicStocks                                       *)
(**************************************************************)

Clear[ModelHeuristicStocks];

SyntaxInformation[ModelHeuristicStocks] = {"ArgumentsPattern" -> {_, _., OptionsPattern[]}};

ModelHeuristicStocks::nargs = "The first argument is expected to be a list of equations. \
The second argument is expected to be symbol.";

ModelHeuristicStocks[ model_?EpidemiologyModelQ ] := Head /@ Keys[model["Stocks"]];

ModelHeuristicStocks[ lsEquations : {_Equal..}, tvar_Symbol ] :=
    Block[{lsLHS, lsSymbols},

      lsLHS = First /@ lsEquations;

      lsSymbols =
          Union @
              Join[
                Cases[ lsLHS, (f_[tvar]) :> f, Infinity],
                Cases[ lsLHS, (f_[s___][tvar]) :> f[s], Infinity],

                Cases[ lsLHS, (Derivative[1][f_][tvar]) :> f, Infinity],
                Cases[ lsLHS, (Derivative[1][f_[s___]][tvar]) :> f[s], Infinity],

                Cases[ lsLHS, (Subscript[s___][tvar]) :> Subscript[s], Infinity]
              ];
      DeleteCases[lsSymbols, Derivative[__][__] ]
    ];

ModelHeuristicStocks[___] :=
    Block[{},
      Message[ModelHeuristicStocks::nargs];
      $Failed
    ];


(**************************************************************)
(* SystemDynamicsModelGraph                                   *)
(**************************************************************)

Clear[SystemDynamicsModelGraph];

SyntaxInformation[SystemDynamicsModelGraph] = {"ArgumentsPattern" -> {_, _, _., _., OptionsPattern[]}};

SystemDynamicsModelGraph::nargs = "The first argument is expected to be a list of equations. \
The second argument is expected to be a list of stocks. \
The third argument is expected to be symbol.";

Options[SystemDynamicsModelGraph] =
    { "ExpandEquations" -> False,
      "IncludeEquationIndexes" -> Automatic };

SystemDynamicsModelGraph[ model_?EpidemiologyModelQ, tvar_Symbol, opts : OptionsPattern[]] :=
    SystemDynamicsModelGraph[ model["Equations"], Head /@ Keys[model["Stocks"]], tvar, opts];

SystemDynamicsModelGraph[lsEquations : {_Equal ..}, Automatic, tvar_Symbol, opts : OptionsPattern[]] :=
    SystemDynamicsModelGraph[lsEquations, ModelHeuristicStocks[lsEquations, tvar], tvar, opts];

SystemDynamicsModelGraph[lsEquations : {_Equal ..}, lsStocks_List, tvar_Symbol, opts : OptionsPattern[]] :=
    SystemDynamicsModelGraph[lsEquations, lsStocks, lsStocks, tvar, opts];

SystemDynamicsModelGraph[lsEquations : {_Equal ..}, lsStocksFrom_List, lsStocksTo_List, tvar_Symbol, opts : OptionsPattern[]] :=
    Block[{aLHSIndexes, aRHSIndexes, inclEqIndexesQ = False, lsModelStocks},

      lsModelStocks = Union[lsStocksFrom, lsStocksTo];

      aLHSIndexes =
          Association@
              Map[
                Function[{s},
                  s -> Fold[If[FreeQ[lsEquations[[#2, 1]], s], #1, Append[#1, #2], #1] &, {}, Range[Length[lsEquations]]]
                ],
                lsModelStocks
              ];

      aRHSIndexes =
          Association@
              Map[
                Function[{s},
                  s -> Fold[If[FreeQ[lsEquations[[#2, 2]], s], #1, Append[#1, #2], #1] &, {}, Range[Length[lsEquations]]]],
                lsModelStocks
              ];

      inclEqIndexesQ = OptionValue[SystemDynamicsModelGraph, "IncludeEquationIndexes"];

      If[TrueQ[inclEqIndexesQ === Automatic],
        inclEqIndexesQ = ! Apply[And, Length[#] == 1 & /@ Values[aLHSIndexes]],
        (*ELSE*)
        inclEqIndexesQ = TrueQ[inclEqIndexesQ]
      ];

      Join @@
          Map[
            FocusStockDependencies[lsEquations, lsStocksFrom, aLHSIndexes, aRHSIndexes, #, tvar, "IncludeEquationIndexes" -> inclEqIndexesQ, opts] &,
            lsStocksTo
          ]
    ];

SystemDynamicsModelGraph[___] :=
    Block[{},
      Message[SystemDynamicsModelGraph::nargs];
      $Failed
    ];


(**************************************************************)
(* FocusStockDependencies                                     *)
(**************************************************************)

Clear[FocusStockDependencies];

Options[FocusStockDependencies] = Options[SystemDynamicsModelGraph];

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
                        Flatten[ Join[
                          MapThread[Thread[#2 -> Cases[expandFunc[#1], (Times[f__, s2] | Times[s2, f__]) :> Times[f], Infinity]] &, {lsStockEquations, aLHSIndexes[focusStock]}],
                          MapThread[Thread[#2 -> Cases[expandFunc[#1], (Plus[__, s2, ___] | Plus[___, s2, __]) -> 1, Infinity]] &, {lsStockEquations, aLHSIndexes[focusStock]}]
                        ] ]
                  ]
                ],
                lsStocks
              ];

      aRes = Select[aRes, Length[#] > 0 &];

      If[! inclEqIndexesQ,
        aRes = Map[#[[All, 2]] &, aRes]
      ];

      aRes
    ];


(**************************************************************)
(* ModelDependencyGraphEdges                                  *)
(**************************************************************)

Clear[ModelDependencyGraphEdges];

SyntaxInformation[ModelDependencyGraphEdges] = {"ArgumentsPattern" -> {_, _, _., OptionsPattern[]}};

ModelDependencyGraphEdges::nargs = "The first argument is expected to be a list of equations. \
The second argument is expected to be a list of stocks. \
The third argument is expected to be symbol.";

Options[ModelDependencyGraphEdges] = Options[SystemDynamicsModelGraph];

ModelDependencyGraphEdges[ model_?EpidemiologyModelQ, tvar_Symbol, opts : OptionsPattern[]] :=
    ModelDependencyGraphEdges[ model["Equations"], Head /@ Keys[model["Stocks"]], tvar, opts];

ModelDependencyGraphEdges[lsEquations : {_Equal ..}, Automatic, tvar_Symbol, opts : OptionsPattern[]] :=
    ModelDependencyGraphEdges[lsEquations, ModelHeuristicStocks[lsEquations, tvar], tvar, opts];

ModelDependencyGraphEdges[lsEquations : {_Equal ..}, lsStocks_List, tvar_Symbol, opts : OptionsPattern[]] :=
    ModelDependencyGraphEdges[lsEquations, lsStocks, lsStocks, tvar, opts];

ModelDependencyGraphEdges[lsEquations : {_Equal ..}, lsStocksFrom_List, lsStocksTo_List, tvar_Symbol, opts : OptionsPattern[]] :=
    Flatten @
        KeyValueMap[
          Thread[DirectedEdge[#1[[2]], #[[1]], #2]] &,
          SystemDynamicsModelGraph[lsEquations, lsStocksFrom, lsStocksTo, tvar, FilterRules[{opts}, Options[SystemDynamicsModelGraph]]]
        ];

ModelDependencyGraphEdges[___] :=
    Block[{},
      Message[ModelDependencyGraphEdges::nargs];
      $Failed
    ];


(**************************************************************)
(* ModelDependencyGraph                                       *)
(**************************************************************)

Clear[ModelDependencyGraph];

SyntaxInformation[ModelDependencyGraph] = {"ArgumentsPattern" -> {_, _, _., OptionsPattern[]}};

ModelDependencyGraph::nargs = "The first argument is expected to be a list of equations. \
The second argument is expected to be a list of stocks. \
The third argument is expected to be symbol.";

Options[ModelDependencyGraph] = Join[ Options[SystemDynamicsModelGraph], Options[Graph]];

ModelDependencyGraph[ model_?EpidemiologyModelQ, tvar_Symbol, opts : OptionsPattern[]] :=
    ModelDependencyGraph[ model["Equations"], Head /@ Keys[model["Stocks"]], tvar, opts];

ModelDependencyGraph[lsEquations : {_Equal ..}, Automatic, tvar_Symbol, opts : OptionsPattern[]] :=
    ModelDependencyGraph[lsEquations, ModelHeuristicStocks[lsEquations, tvar], tvar, opts];

ModelDependencyGraph[lsEquations : {_Equal ..}, lsStocks_List, tvar_Symbol, opts : OptionsPattern[]] :=
    ModelDependencyGraph[lsEquations, lsStocks, lsStocks, tvar, opts];

ModelDependencyGraph[lsEquations : {_Equal ..}, lsStocksFrom_List, lsStocksTo_List, tvar_Symbol, opts : OptionsPattern[]] :=
    Block[{lsSSRules},

      lsSSRules = ModelDependencyGraphEdges[lsEquations, lsStocksFrom, lsStocksTo, tvar, opts];

      Graph[
        lsSSRules,
        Sequence @@ FilterRules[{opts}, Options[Graph]],
        GraphLayout -> "GravityEmbedding", VertexLabels -> "Name", EdgeLabels -> "EdgeTag"]
    ];

ModelDependencyGraph[___] :=
    Block[{},
      Message[ModelDependencyGraph::nargs];
      $Failed
    ];


(**************************************************************)
(* ModelGraphEquations                                        *)
(**************************************************************)

Clear[ModelGraphEquations];

SyntaxInformation[ModelGraphEquations] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

ModelGraphEquations::nargs = "The first argument is expected to be a list of directed edges with weights. \
The second argument is expected to be symbol.";

ModelGraphEquations[ lsEdges : { DirectedEdge[ _, _, _] .. }, tvar_Symbol ] :=
    Block[{aGroups},

      aGroups = GroupBy[ lsEdges, #[[2]]&, List @@@ #&];

      KeyValueMap[ Derivative[1][#1][tvar] == Dot[ Through[ #2[[All,1]] [tvar] ], #2[[All,3]] ]&, aGroups]

    ];

ModelGraphEquations[___] :=
    Block[{},
      Message[ModelGraphEquations::nargs];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]