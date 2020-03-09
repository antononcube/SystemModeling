(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

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

AddCellIdentifier::usage = "AddCellIdentifier[m, id] adds a specified identifier id to all stocks and rates on the model m.";

MakeCoreMultiCellModel::usage = "MakeCoreMultiCellModel[coreModel_, n_Integer | ids_List, t_Symbol, context_] \
makes core multi-cell model.";

EquationPosition::usage = "EquationPosition[eqs:{_Equal..}, lhs_] finds the element of eqs that has \
the specified left hand side lhs.";

AddTermsToEquations::usage = "AddTermsToEquations[eqs:{_Equal..}, nt_Association] adds the corresponding new terms in
specified with nt to the right hand side of the equations eqs.";

MakeMigrationTerms::usage = "MakeMigrationTerms[m_?MatrixQ, TPs_List, Ps_List] gives an association with \
the migration terms for the total populations TPs and populations Ps.";

ToGeoCompartmentsModel::usage = "ToGeoCompartmentsModel[singleCellModel_Association, mat_?MatrixQ, opts___] \
makes a multi-cell model based on singleCellModel using the population migration matrix mat.";

Begin["`Private`"];

(***********************************************************)
(* Add ID                                                  *)
(***********************************************************)

Clear[AddCellIdentifier];

AddCellIdentifier[ model_Association, id_ ] :=
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

Clear[MakeCoreMultiCellModel];

MakeCoreMultiCellModel::"nargs" = "The first argument is expected to be a model association or a model making function. \
The second argument is expected to be an integer or a list of ID's. \
If the first argument is model making function the third argument is expected to be a time variable, \
and the fourth (optional) argument is expected to be a context.";

MakeCoreMultiCellModel[model : (_Symbol | _Association), n_Integer, args___] :=
    MakeCoreMultiCellModel[model, Range[n], args];

MakeCoreMultiCellModel[model_Symbol, cellIDs_List, t_Symbol, context_ : "Global`"] :=
    MapThread[Join, Map[AddCellIdentifier[model[t, context], #] &, cellIDs]];

MakeCoreMultiCellModel[model_Association, cellIDs_List, args___] :=
    MapThread[Join, Map[AddCellIdentifier[model, #] &, cellIDs]];

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
(* GeoCompartmentsModel                                    *)
(***********************************************************)

Clear[GetPopulations, GetPopulationSymbols];

GetPopulations[model_Association, descr_String] :=
    Keys[Select[model["Stocks"], # == descr &]];

GetPopulationSymbols[model_Association, descr_String] :=
    Cases[GetPopulations[model, descr], p_[id_][_] :> p[id]];


(*matMigration - constanst matrix*)
(*matMigration - time depedent*)
(*matMigration -  *)

Clear[ToGeoCompartmentsModel];

ToGeoCompartmentsModel::"nargs" = "The first argument is expected to be a single cell model association. \
The second argument is expected to be a square matrix. \
The third optional argument is expected to be list of ID's with length that corresponds to number of rows of the first argument.";

ToGeoCompartmentsModel::"nmgrpop" = "The values of the option \"MigratingPopulations\" is expected to be
a subset of `1` or one of Automatic, All, or None.";

Options[ToGeoCompartmentsModel] = {"MigratingPopulations" -> Automatic};

ToGeoCompartmentsModel[model_Association, matMigration_?MatrixQ, opts : OptionsPattern[] ] :=
    Block[{ids},
      ids = Range @ Dimensions[matMigration][[1]];
      ToGeoCompartmentsModel[model, matMigration, ids, opts]
    ] /; (Equal @@ Dimensions[matMigration]);

(*ToGeoCompartmentsModel[matMigration_?SSparseMatrixQ] :=*)
(*    Block[{},*)
(*    ];*)

ToGeoCompartmentsModel[model_Association, matMigration_?MatrixQ, cellIDs_List, opts : OptionsPattern[] ] :=
    Block[{allPops, migrPops, coreModel, eqs, newTerms},

      migrPops = OptionValue[ToGeoCompartmentsModel, "MigratingPopulations"];

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
        Message[ToGeoCompartmentsModel::"nmgrpop", allPops];
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

ToGeoCompartmentsModel[___] :=
    Block[{},
      Message[ToGeoCompartmentsModel::"nargs"];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]