(*
    Epidemiology models modifications Mathematica package
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


(* :Title: EpidemiologyModelModifications *)
(* :Context: EpidemiologyModelModifications` *)
(* :Author: antonov *)
(* :Date: 2020-03-08 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 antonov *)
(* :Keywords: *)
(* :Discussion: *)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[EpidemiologyModels`SIRModel]] == 0,
  Echo["EpidemiologyModels.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["EpidemiologyModelModifications`"];
(* Exported symbols added here with SymbolName::usage *)

AddModelIdentifier::usage = "AddModelIdentifier[m, id] adds a specified identifier id to all stocks and rates on the model m.";

JoinModels::usage = "JoinModels[m1_Association, ..] or JoinModels[ {_Association ..} ] joins models.";

MakeCoreMultiCellModel::usage = "MakeCoreMultiCellModel[coreModel_, n_Integer | ids_List, t_Symbol, context_] \
makes core multi-cell model.";

EquationPosition::usage = "EquationPosition[eqs:{_Equal..}, lhs_] finds the element of eqs that has \
the specified left hand side lhs.";

AddTermsToEquations::usage = "AddTermsToEquations[eqs:{_Equal..}, nt_Association] adds the corresponding new terms in
specified with nt to the right hand side of the equations eqs.";

MakeMigrationTerms::usage = "MakeMigrationTerms[m_?MatrixQ, TPs_List, Ps_List] gives an association with \
the migration terms for the total populations TPs and populations Ps.";

GetPopulations::usage = "GetPopulations[m_Association, d_String] get populations in the model m \
that correspond to the descriptions d.";

GetPopulationSymbols::usage = "GetPopulationSymbols[m_Association, d_String] get population symbols in the model m \
that correspond to the descriptions d.";

GetRates::usage = "GetRates[m_Association, d_String] get rates in the model m \
that correspond to the descriptions d.";

GetRatesSymbols::usage = "GetRatesSymbols[m_Association, d_String] get rates symbols in the model m \
that correspond to the descriptions d.";

ToSiteCompartmentsModel::usage = "ToSiteCompartmentsModel[singleCellModel_Association, mat_?MatrixQ, opts___] \
makes a multi-cell model based on singleCellModel using the population migration matrix mat.";

SetInitialConditions::usage = "SetInitialConditions[ m_Association, ics_Associations] changes the initial
conditions of the model m according to the rules ics.";

SetRateRules::usage = "SetRateRules[ m_Association, rrs_Associations] changes the rate rules
of the model m according to the rules rrs.";

Begin["`Private`"];

(***********************************************************)
(* Add ID                                                  *)
(***********************************************************)

Clear[AddModelIdentifier];

AddModelIdentifier[ model_Association, id_ ] :=
    Block[{modelSymbols, rules},
      modelSymbols = Union @ Cases[ Normal /@ Values[ KeyTake[model, {"Stocks", "Rates"}]], HoldPattern[ (x_Symbol | x_[args__]) -> descr_String ] :> x , Infinity ];

      rules = {
        (x_Symbol[args__] /; MemberQ[modelSymbols, x]) :> x[id][args],
        Derivative[d_][x_Symbol][args__] :> Derivative[d][x[id]][args],
        (x_Symbol /; MemberQ[modelSymbols, x]) :> x[id]
      };

      Association @ KeyValueMap[ #1 -> If[ AssociationQ[#2], Association[ Normal[#2] /. rules], #2 /. rules ] &, model ]
    ];

(***********************************************************)
(* MakeCoreMultiCellModel                                  *)
(***********************************************************)

Clear[JoinModels];

JoinModels::"nargs" = "The arguments are expected to be valid model associations.";

JoinModels[ m1_Association, args__ ] :=
    JoinModels[ {m1, args} ];

JoinModels[ models : {_Association..} ] :=
    MapThread[Join, models];

JoinModels[___] :=
    Block[{},
      Message[JoinModels::"nargs"];
      $Failed
    ];

(***********************************************************)
(* MakeCoreMultiCellModel                                  *)
(***********************************************************)

Clear[MakeCoreMultiCellModel];

MakeCoreMultiCellModel::"nargs" = "The first argument is expected to be a model association or a model making function. \
The second argument is expected to be an integer or a list of ID's. \
If the first argument is model making function the third argument is expected to be a time variable, \
and the fourth (optional) argument is expected to be a context.";

MakeCoreMultiCellModel[model : (_Symbol | _Association), n_Integer, args___] :=
    MakeCoreMultiCellModel[model, Range[n], args];

MakeCoreMultiCellModel[model_Symbol, cellIDs_List, t_Symbol, context_ : "Global`"] :=
    MapThread[Join, Map[AddModelIdentifier[model[t, context], #] &, cellIDs]];

MakeCoreMultiCellModel[model_Association, cellIDs_List, args___] :=
    MapThread[Join, Map[AddModelIdentifier[model, #] &, cellIDs]];

MakeCoreMultiCellModel[___] :=
    Block[{},
      Message[MakeCoreMultiCellModel::"nargs"];
      $Failed
    ];

(***********************************************************)
(* MakeMigrationTerms                                      *)
(***********************************************************)

Clear[MakeMigrationTerms];

MakeMigrationTerms[mat_?MatrixQ, TPs_List, Ps_List, t_Symbol] :=
    MakeMigrationTerms[mat, Through[TPs, t], Through[Ps[t]] ];

MakeMigrationTerms[mat_?MatrixQ, TPs_List, Ps_List] :=
    Block[{n = Dimensions[mat][[2]], res},
      res =
          Total /@
              Association[
                Table[
                  Ps[[i]] ->
                      Table[Ps[[j]] / TPs[[j]] * mat[[j, i]] - Ps[[i]] / TPs[[i]] * mat[[i, j]], {j, n}], {i, n}]];

      res = AssociationThread[ Keys[res] /. p_[id_][__] :> p[id], Values[res]];

      res

    ] /; Dimensions[mat][[1]] == Dimensions[mat][[2]] == Length[TPs] == Length[Ps];

MakeMigrationTerms[___] := $Failed;


(***********************************************************)
(* Locate equation                                         *)
(***********************************************************)

Clear[EquationPosition];

EquationPosition[model_Association, lhsSpec_] := EquationPosition[model["Equations"], lhsSpec];

EquationPosition[equations : {_Equal ..}, lhsSpec_] :=
    Block[{pos},
      pos = Flatten[Position[equations[[All, 1]], lhsSpec]];
      If[Length[pos] == 0, $Failed, First[pos]]
    ];

EquationPosition[___] := $Failed;

(***********************************************************)
(* Add terms to equation                                   *)
(***********************************************************)

Clear[AddTermsToEquations];

AddTermsToEquations[equations : {_Equal ..}, lhsSpec_ -> terms_] :=
    Block[{pos = EquationPosition[equations, lhsSpec]},

      If[TrueQ[pos === $Failed], Return[$Failed]];

      ReplacePart[equations, pos -> equations[[pos, 1]] == equations[[pos, 2]] + terms]
    ];

AddTermsToEquations[equations : {_Equal ..}, newTerms_Association] :=
    Fold[AddTermsToEquations, equations, Normal@newTerms];

AddTermsToEquations[___] := $Failed;


(***********************************************************)
(* Get populations symbols                                 *)
(***********************************************************)

Clear[GetPopulations, GetPopulationSymbols];

GetPopulations[model_Association, descr_String] :=
    Keys[Select[model["Stocks"], # == descr &]];

GetPopulationSymbols[model_Association, descr_String] :=
    Join[
      Cases[GetPopulations[model, descr], p_Symbol[id_][_] :> p[id] ],
      Cases[GetPopulations[model, descr], p_Symbol[_] :> p ]
    ];


(***********************************************************)
(* Get rates symbols                                       *)
(***********************************************************)

Clear[GetRates, GetRateSymbols];

GetRates[model_Association, descr_String] :=
    Keys[Select[model["Rates"], # == descr &]];

GetRateSymbols[model_Association, descr_String] :=
    Join[
      Cases[GetRates[model, descr], p_Symbol[id_][_] :> p[id] ],
      Cases[GetRates[model, descr], p_Symbol[_] :> p ]
    ];


(***********************************************************)
(* GeoCompartmentsModel                                    *)
(***********************************************************)

(*matMigration - constanst matrix*)
(*matMigration - time depedent*)
(*matMigration -  *)

Clear[ToSiteCompartmentsModel];

ToSiteCompartmentsModel::"nargs" = "The first argument is expected to be a single cell model association. \
The second argument is expected to be a square matrix. \
The third optional argument is expected to be list of ID's with length that corresponds to number of rows of the first argument.";

ToSiteCompartmentsModel::"nmgrpop" = "The values of the option \"MigratingPopulations\" is expected to be
a subset of `1` or one of Automatic, All, or None.";

Options[ToSiteCompartmentsModel] = {"MigratingPopulations" -> Automatic};

ToSiteCompartmentsModel[model_Association, matMigration_?MatrixQ, opts : OptionsPattern[] ] :=
    Block[{ids},
      ids = Range @ Dimensions[matMigration][[1]];
      ToSiteCompartmentsModel[model, matMigration, ids, opts]
    ] /; (Equal @@ Dimensions[matMigration]);

(*ToGeoCompartmentsModel[matMigration_?SSparseMatrixQ] :=*)
(*    Block[{},*)
(*    ];*)

ToSiteCompartmentsModel[model_Association, matMigration_?MatrixQ, cellIDs_List, opts : OptionsPattern[] ] :=
    Block[{allPops, migrPops, coreModel, eqs, newTerms},

      migrPops = OptionValue[ToSiteCompartmentsModel, "MigratingPopulations"];

      allPops = Values @ Select[ model["Stocks"], StringMatchQ[#, __ ~~ "Population" ~~ EndOfString ]& ];
      allPops = Complement[allPops, {"Total Population"} ];

      Which[
        TrueQ[migrPops === None],
        migrPops = {},

        TrueQ[migrPops === Automatic],
        migrPops = Complement[ allPops, {"Infected Severely Symptomatic Population"} ],

        TrueQ[migrPops === Automatic] || TrueQ[migrPops === All],
        migrPops = allPops,

        VectorQ[migrPops, StringQ] && Length[ Intersection[allPops, migrPops] ] == Length[ Union[migrPops] ],
        migrPops = Union[migrPops],

        True,
        Message[ToSiteCompartmentsModel::"nmgrpop", ToString[InputForm[allPops]] ];
        Return[$Failed]
      ];

      coreModel = MakeCoreMultiCellModel[model, cellIDs];

      If[ Length[migrPops] == 0,
        Return[coreModel]
      ];

      eqs = coreModel["Equations"];

      eqs =
          Fold[(
            newTerms =
                MakeMigrationTerms[
                  matMigration,
                  GetPopulations[coreModel, "Total Population"],
                  GetPopulations[coreModel, #2]
                ];
            AddTermsToEquations[#1, newTerms])&,
            eqs,
            migrPops
          ];

      Join[ coreModel, <| "Equations" -> eqs |> ]

    ] /; Dimensions[matMigration][[1]] == Dimensions[matMigration][[2]] == Length[cellIDs];

ToSiteCompartmentsModel[___] :=
    Block[{},
      Message[ToSiteCompartmentsModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* Initial conditions setter                               *)
(***********************************************************)

Clear[SetInitialConditions];

SetInitialConditions::"nargs" = "The first argument is expected to be a model association. \
The second argument is expected to be an associations of initial condition rules.";

SetInitialConditions::"ninit" = "The model does not have initial conditions.";

SetInitialConditions[model_Association, aInitConds_Association] :=
    Block[{lsInitConds, pos},

      If[ !KeyExistsQ[model, "InitialConditions"],
        Message[SetInitialConditions::"ninit"];
        Return[$Failed]
      ];

      lsInitConds = model["InitialConditions"];

      lsInitConds =
          Fold[
            Function[{ics, icRule},
              pos = EquationPosition[lsInitConds, icRule[[1]]];
              If[IntegerQ[pos],
                ReplacePart[ics, pos -> icRule[[1]] == icRule[[2]]]
              ]],
            lsInitConds,
            Normal[aInitConds]
          ];

      Join[model, <|"InitialConditions" -> lsInitConds|>]
    ];

SetInitialConditions[___] :=
    Block[{},
      Message[SetInitialConditions::"nargs"];
      $Failed
    ];


(***********************************************************)
(* Rate rules setter                                       *)
(***********************************************************)

Clear[SetRateRules];

SetRateRules::"nargs" = "The first argument is expected to be a model association. \
The second argument is expected to be an association of rate rules.";

SetRateRules::"nrrs" = "The model does not have rate rules.";

SetRateRules[model_Association, aRateRules_Association] :=
    Block[{lsRateRules},

      If[ !KeyExistsQ[model, "RateRules"],
        Message[SetRateRules::"nrrs"];
        Return[$Failed]
      ];

      lsRateRules = Join[ model["RateRules"], aRateRules ] ;

      Join[model, <|"RateRules" -> lsRateRules|>]
    ];

SetRateRules[___] :=
    Block[{},
      Message[SetRateRules::"nargs"];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]