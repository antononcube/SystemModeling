(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: EpidemiologyModels-NDSolve-Unit-Tests *)
(* :Context: EpidemiologyModels-NDSolve-Unit-Tests` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-04-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

(*BeginPackage["EpidemiologyModels-NDSolve-Unit-Tests`"];*)
(* Exported symbols added here with SymbolName::usage *)

(*Begin["`Private`"];*)

Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m"];
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelModifications.m"];
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelingVisualizationFunctions.m"];
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelingSimulationFunctions.m"];
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/WL/SystemDynamicsInteractiveInterfacesFunctions.m"];

Clear[NDSolveCheckRuns];
NDSolveCheckRuns[func_, testIDPrefix_String, opts : OptionsPattern[]] :=
    Block[{modelSIR},
      {
        VerificationTest[
          MemberQ[Join[Names["SI*Model"], Names["SEI*Model"]], ToString[func]],
          True,
          TestID -> testIDPrefix <> "-KnownSymbol"],

        VerificationTest[
          modelSIR = func[t, opts];
          AssociationQ[modelSIR],
          True,
          TestID -> testIDPrefix <> "-MakeModel"],

        VerificationTest[
          AssociationQ[ModelGridTableForm[modelSIR]],
          True,
          TestID -> testIDPrefix <> "-ModelGridTableForm"],

        VerificationTest[
          lsActualEquations0 = ModelNDSolveEquations[modelSIR];
          MatchQ[lsActualEquations0, {_Equal ..}],
          True,
          TestID -> testIDPrefix <> "-ModelNDSolveEquations"],

        VerificationTest[
          res =
              NDSolve[lsActualEquations0, GetStockSymbols[modelSIR], {t, 0, 365}];
          MatchQ[res, {{(_ -> (TP | _InterpolatingFunction)) ..}}],
          True,
          TestID -> testIDPrefix <> "-NDSolve"]
      }
    ];

lsTestRes =
    Flatten[
      Outer[
        NDSolveCheckRuns[#1, ToString[#1] <> "-" <> #2,
          "InitialConditions" -> True,
          "RateRules" -> True,
          "TotalPopulationRepresentation" -> #2] &,
        {SIRModel, SI2RModel, SEI2RModel, SEI2HRModel, SEI2HREconModel},
        {"Constant", "SumSubstitution", "AlgebraicEquation"}]
    ];

lsTestRes

(*End[]; *)(* `Private` *)

(*EndPackage[]*)