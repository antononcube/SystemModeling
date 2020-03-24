(*
    Epidemiology model modifications Mathematica package
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
(* :Author: Anton Antonov *)
(* :Date: 2020-03-08 *)

(* :Package Version: 0.3 *)
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


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["EpidemiologyModelModifications`"];
(* Exported symbols added here with SymbolName::usage *)

(*AddDeceasedPopulation::usage "AddDeceasedPopulation[model] add a deceased equation and stock to the model argument.";*)

AddModelIdentifier::usage = "AddModelIdentifier[m, id] adds a specified identifier id to all stocks and rates on the model m.";

JoinModels::usage = "JoinModels[m1_Association, ..] or JoinModels[ {_Association ..} ] joins models.";

MakeCoreMultiSiteModel::usage = "MakeCoreMultiSiteModel[coreModel_, n_Integer | ids_List, t_Symbol, context_] \
makes core multi-cell model.";

EquationPosition::usage = "EquationPosition[eqs:{_Equal..}, lhs_] finds the element of eqs that has \
the specified left hand side lhs.";

AddTermsToEquations::usage = "AddTermsToEquations[eqs:{_Equal..}, nt_Association] adds the corresponding new terms in
specified with nt to the right hand side of the equations eqs.";

MakeMigrationTerms::usage = "MakeMigrationTerms[m_?MatrixQ, TPs_List, Ps_List] gives an association with \
the migration terms for the total populations TPs and populations Ps.";

GetStocks::usage = "GetStocks[m_Association, d:(_String | _StringExpression)] get stocks
in the model m that correspond to the descriptions d.";

GetStockSymbols::usage = "GetStockSymbols[m_Association, d:(_String | _StringExpression)] get
stock symbols in the model m that correspond to the descriptions d.";

GetPopulations::usage = "GetPopulations[m_Association, d:(_String | _StringExpression)] get populations
in the model m that correspond to the descriptions d. A synonym of GetStocks.";

GetPopulationSymbols::usage = "GetPopulationSymbols[m_Association, d:(_String | _StringExpression)] get
population symbols in the model m that correspond to the descriptions d. A synonym of GetStockSymbols.";

GetRates::usage = "GetRates[m_Association, d:(_String | _StringExpression)] get rates in the model m \
that correspond to the descriptions d.";

GetRateSymbols::usage = "GetRateSymbols[m_Association, d:(_String | _StringExpression)] get rates symbols in
the model m that correspond to the descriptions d.";

ToSiteCompartmentsModel::usage = "ToSiteCompartmentsModel[singleCellModel_Association, mat_?MatrixQ, opts___] \
makes a multi-cell model based on singleCellModel using the population migration matrix mat.";

SetInitialConditions::usage = "SetInitialConditions[ m_Association, ics_Associations] changes the initial
conditions of the model m according to the rules ics.";

SetRateRules::usage = "SetRateRules[ m_Association, rrs_Associations] changes the rate rules
of the model m according to the rules rrs.";

ToAssociation::usage = "ToAssociation[ eqs : { _Equal..} ] converts a list equations into an association.";

Begin["`Private`"];

Needs["EpidemiologyModels`"];

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
(* JoinModels                                              *)
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

Clear[MakeCoreMultiSiteModel];

MakeCoreMultiSiteModel::"nargs" = "The first argument is expected to be a model association or a model making function. \
The second argument is expected to be an integer or a list of ID's. \
If the first argument is model making function the third argument is expected to be a time variable, \
and the fourth (optional) argument is expected to be a context.";

MakeCoreMultiSiteModel[model : (_Symbol | _Association), n_Integer, args___] :=
    MakeCoreMultiSiteModel[model, Range[n], args];

MakeCoreMultiSiteModel[model_Symbol, cellIDs_List, t_Symbol, context_ : "Global`"] :=
    MapThread[Join, Map[AddModelIdentifier[model[t, context], #] &, cellIDs]];

MakeCoreMultiSiteModel[model_Association, cellIDs_List, args___] :=
    MapThread[Join, Map[AddModelIdentifier[model, #] &, cellIDs]];

MakeCoreMultiSiteModel[___] :=
    Block[{},
      Message[MakeCoreMultiSiteModel::"nargs"];
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
                      Table[
                        Which[

                          TrueQ[mat[[j, i]] == 0] && TrueQ[mat[[i, j]] == 0],
                          0,

                          TrueQ[mat[[j, i]] == 0],
                          - Min[ Ps[[i]] / TPs[[i]] * mat[[i, j]], TPs[[i]] ],

                          TrueQ[mat[[i, j]] == 0],
                          Min[ Ps[[j]] / TPs[[j]] * mat[[j, i]], TPs[[j]] ],

                          True,
                          Min[ Ps[[j]] / TPs[[j]] * mat[[j, i]], TPs[[j]] ] - Min[ Ps[[i]] / TPs[[i]] * mat[[i, j]], TPs[[i]] ]
                        ],
                        {j, n}],
                  {i, n}]
              ];

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
(* Get model entity symbols                                *)
(***********************************************************)

Clear[RetrieveModelEntities];

SyntaxInformation[RetrieveModelEntities] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[RetrieveModelEntities] = Options[StringMatchQ];

RetrieveModelEntities[model_Association, key_, opts : OptionsPattern[]] :=
    RetrieveModelEntities[model, key, __ ~~ __, opts];

RetrieveModelEntities[model_Association, key_, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    Keys[Select[model[key], StringMatchQ[#, descr, opts]&]];


Clear[RetrieveModelEntitySymbols];

SyntaxInformation[RetrieveModelEntitySymbols] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[RetrieveModelEntitySymbols] = Options[RetrieveModelEntities];

RetrieveModelEntitySymbols[model_Association, key_, opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[model, key, __ ~~ __, opts];

RetrieveModelEntitySymbols[model_Association, key_, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    Join[
      Cases[RetrieveModelEntities[model, key, descr, opts], p_Symbol[id_][_] :> p[id] ],
      Cases[RetrieveModelEntities[model, key, descr, opts], p_Symbol[_] :> p ]
    ];


(***********************************************************)
(* Get stocks symbols                                      *)
(***********************************************************)

Clear[GetStocks];

SyntaxInformation[GetStocks] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[GetStocks] = Options[StringMatchQ];

GetStocks[ model_Association, opts : OptionsPattern[] ] := GetStocks[ model, __ ~~ __, opts ];

GetStocks[ model_Association, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    RetrieveModelEntities[ model, "Stocks", descr, opts];


Clear[GetStockSymbols];

SyntaxInformation[GetStockSymbols] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

GetStockSymbols[ model_Association, opts : OptionsPattern[] ] := GetStockSymbols[ model, __ ~~ __, opts ];

GetStockSymbols[ model_Association, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[ model, "Stocks", descr, opts];


Clear[GetPopulations, GetPopulationSymbols];

GetPopulations = GetStocks;
GetPopulationSymbols = GetStockSymbols;


(***********************************************************)
(* Get rates symbols                                       *)
(***********************************************************)

Clear[GetRates];

SyntaxInformation[GetRates] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[GetRates] = Options[StringMatchQ];

GetRates[ model_Association, opts : OptionsPattern[] ] := GetRates[ model, __ ~~ __, opts ];

GetRates[ model_Association, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    RetrieveModelEntities[ model, "Rates", descr, opts];


Clear[GetRateSymbols];

SyntaxInformation[GetRateSymbols] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

GetRateSymbols[ model_Association, opts : OptionsPattern[] ] := GetRateSymbols[ model, __ ~~ __, opts ];

GetRateSymbols[ model_Association, descr : (_String | _StringExpression), opts : OptionsPattern[] ] :=
    RetrieveModelEntitySymbols[ model, "Rates", descr, opts];


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

      coreModel = MakeCoreMultiSiteModel[model, cellIDs];

      If[ Length[migrPops] == 0,
        Return[coreModel]
      ];

      eqs = coreModel["Equations"];

      eqs =
          Fold[(
            newTerms =
                MakeMigrationTerms[
                  matMigration,
                  GetStocks[coreModel, "Total Population"],
                  GetStocks[coreModel, #2]
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

SetInitialConditions[model_Association, lsInitConds : { _Equal .. } ] :=
    SetInitialConditions[ model, Association @ ReplaceAll[ lsInitConds, Equal[x_, y_] :> Rule[x, y] ] ];

SetInitialConditions[model_Association, aInitConds_Association] :=
    Block[{lsInitConds, pos},

      If[ !KeyExistsQ[model, "InitialConditions"],

        lsInitConds = KeyValueMap[ Equal[#1, #2]&, aInitConds],
        (* ELSE *)

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
            ]
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
        lsRateRules = aRateRules,
        (* ELSE *)
        lsRateRules = Join[ model["RateRules"], aRateRules ]
      ];

      Join[model, <|"RateRules" -> lsRateRules|>]
    ];

SetRateRules[___] :=
    Block[{},
      Message[SetRateRules::"nargs"];
      $Failed
    ];


(***********************************************************)
(* Add ID                                                  *)
(***********************************************************)

Clear[ToAssociation];

ToAssociation::"nargs" = "The first argument is expected to be a list of equations.";

(*ToAssociation[ eqs:{ _Equal ..} ] := Association@ToRules[And @@ eqs];*)
ToAssociation[ eqs : { _Equal ..} ] := Association[ Rule @@@ eqs ];

ToAssociation[___] :=
    Block[{},
      Message[ToAssociation::"nargs"];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]