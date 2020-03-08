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

AddIdentifier::usage = "AddIdentifier[m, id] adds a specified identifier id to all stocks and rates on the model m.";

MakeCoreMultiCellModel::usage = "MakeCoreMultiCellModel[coreModel_, n_Integer | ids_List, t_Symbol, context_] \
makes core multi-cell model.";

EquationPosition::usage = "EquationPosition[eqs:{_Equal..}, lhs_] finds the element of eqs that has \
the specified left hand side lhs.";

AddTermsToEquation::usage = "AddTermsToEquation[eqs:{_Equal..}, nt_Association] adds the corresponding new terms in
specified with nt to the right hand side of the equations eqs.";

ToGeoCompartmentsModel::usage = "ToGeoCompartmentsModel[singleCellModel_Association, mat_?MatrixQ, opts___] \
makes a multi-cell model based on singleCellModel using the population migration matrix mat.";

Begin["`Private`"];

(***********************************************************)
(* Add ID                                                  *)
(***********************************************************)

Clear[AddIdentifier];

AddIdentifier[ model_Association, id_ ] :=
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
    MapThread[Join, Map[AddIdentifier[model[t, context], #] &, cellIDs]];

MakeCoreMultiCellModel[model_Association, cellIDs_List, args___] :=
    MapThread[Join, Map[AddIdentifier[model, #] &, cellIDs]];

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
    Block[{n = Dimensions[mat][[2]]},
      Total /@
          Association[
            Table[
              Ps[[i]] ->
                  Table[Ps[[j]][t] / TPs[[j]][t] * mat[[j, i]] - Ps[[i]][t] / TPs[[i]][t] * mat[[i, j]], {j, n}], {i, n}]]
    ] /; Dimensions[mat][[1]] == Dimensions[mat][[2]] == Length[TPs] == Length[Ps];


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


(***********************************************************)
(* Add terms to equation                                   *)
(***********************************************************)

Clear[AddTermsToEquation];

AddTermsToEquation[equations : {_Equal ..}, lhsSpec_ -> terms_] :=
    Block[{pos = EquationPosition[equations, lhsSpec]},

      If[TrueQ[pos === $Failed], Return[$Failed]];

      ReplacePart[equations, pos -> equations[[pos, 1]] == equations[[pos, 2]] + terms]
    ];

AddTermsToEquation[equations : {_Equal ..}, newTerms_Association] :=
    Fold[AddTermsToEquation, equations, Normal@newTerms];


(***********************************************************)
(* GeoCompartmentsModel                                    *)
(***********************************************************)

(*matMigration - constanst matrix*)
(*matMigration - time depedent*)
(*matMigration -  *)

Clear[ToGeoCompartmentsModel];

GeoCompartmentsModel::"nargs" = "The first argument is expected to be a single cell model association. \
The second argument is expected to be a square matrix. \
The third optional argument is expected to be list of ID's with length that corresponds to number of rows of the first argument.";

ToGeoCompartmentsModel::"nmgrpop" = "At this point only Automatic is implemented for the option \"MigratingPopulations\".";

(* The values of the option "MigratingPopulations" is expected to be
   a subset of {"SP","INSP","ISSP","RP"}
   or one of Automatic, All, or None. *)

Options[GeoCompartmentsModel] = {"MigratingPopulations" -> Automatic};

GeoCompartmentsModel[model_Association, matMigration_?MatrixQ, args___ ] :=
    Block[{ids},
      ids = Range[Dimensions[matMigration[[1]]]];
      ToGeoCompartmentsModel[matMigration, ids, args]
    ] /; (Equal @@ Dimensions[matMigration]);

(*ToGeoCompartmentsModel[matMigration_?SSparseMatrixQ] :=*)
(*    Block[{},*)
(*    ];*)

GeoCompartmentsModel[model_Association, matMigration_?MatrixQ, cellIDs_List, opts : OptionsPattern[] ] :=
    Block[{migrPops, coreModel},

      migrPops = OptionValue[ToGeoCompartmentsModel, "MigratingPopulations"];

      If[ !TrueQ[migrPops === Automatic],
        Message[ToGeoCompartmentsModel::"nmgrpop"];
        Return[$Failed]
      ];

      coreModel = MakeCoreMultiCellModel[model, cellIDs];

    ] /; Dimensions[matMigration][[1]] == Dimensions[matMigration][[2]] == Length[cellIDs];

ToGeoCompartmentsModel[___] :=
    Block[{},
      Message[ToGeoCompartmentsModel::"nargs"];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]