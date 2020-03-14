(*
    Epidemiology models unit tests in Mathematica
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
    antononcube@gmail.com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: EpidemiologyModels-Unit-Tests *)
(* :Context:  *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-14 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Epidemiology model, unit test *)
(* :Discussion:

   This test files has unit tests for the functions in the package:

      https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m

*)

BeginTestSection["EpidemiologyModels-Unit-Tests.mt"];

VerificationTest[
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m"],
    Greater[Length[DownValues[EpidemiologyModels`SEI2RModel]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];

VerificationTest[

  Clear[ShortModelQ];
  ShortModelQ[model_] :=
      AssociationQ[model] &&
          Sort[Keys[model]] == Sort[{ "Stocks", "Rates", "Equations" }] &&
          AssociationQ[model["Stocks"]] &&
          AssociationQ[model["Rates"]] &&
          MatchQ[model["Equations"], { _Equal .. }];

  Clear[LongModelQ];
  LongModelQ[model_] :=
      AssociationQ[model] &&
          Sort[Keys[model]] == Sort[{ "Stocks", "Rates", "Equations", "InitialConditions", "RateRules" }] &&
          AssociationQ[model["Stocks"]] &&
          AssociationQ[model["Rates"]] &&
          MatchQ[model["Equations"], { _Equal .. }] &&
          AssociationQ[model["RateRules"]] &&
          MatchQ[model["InitialConditions"], { _Equal .. }];
  ,
  Null
  ,
  TestID -> "Define-model-test-functions"
];


(***********************************************************)
(* Generate SIR model                                      *)
(***********************************************************)

VerificationTest[
  ShortModelQ[ SIRModel[t] ]
  ,
  True
  ,
  TestID -> "SIRModel-1"
];


VerificationTest[
  ShortModelQ[ SIRModel[t, "MyPrivate`Context1`"] ]
  ,
  True
  ,
  TestID -> "SIRModel-2"
];


VerificationTest[
  LongModelQ[ SIRModel[t, "InitialConditions" -> True, "RateRules" -> True ] ]
  ,
  True
  ,
  TestID -> "SIRModel-3"
];


VerificationTest[
  And @@
      Map[
        LongModelQ[ SIRModel[t, "TotalPopulationRepresentation" -> #, "InitialConditions" -> True, "RateRules" -> True ] ]&,
        { Constant, "AlgebraicEquation", "SumSubstitution" }
      ]
  ,
  True
  ,
  TestID -> "SIRModel-TotalPopulationRepresentation-1"
];


VerificationTest[
  model = SIRModel[t, "TotalPopulationRepresentation" -> Constant, "InitialConditions" -> True, "RateRules" -> True ];
  LongModelQ[model] &&
      ! MemberQ[model["InitialConditions"][[All, 1]], Keys[Select[model["Stocks"], # == "Total Population" &]][[1]]] &&
      MemberQ[Keys[model["RateRules"]], Keys[Select[model["Stocks"], # == "Total Population" &]][[1]]]
  ,
  True
  ,
  TestID -> "SIRModel-TotalPopulationRepresentation-Constant-1"
];


VerificationTest[
  model = SIRModel[t, "TotalPopulationRepresentation" -> "AlgebraicEquation", "InitialConditions" -> True, "RateRules" -> True ];
  LongModelQ[model] &&
      MemberQ[model["Equations"][[All, 1]], Keys[Select[model["Stocks"], # == "Total Population" &]][[1]]] &&
      MemberQ[model["InitialConditions"][[All, 1]], Keys[Select[model["Stocks"], # == "Total Population" &]][[1]] /. t -> 0]
  ,
  True
  ,
  TestID -> "SIRModel-TotalPopulationRepresentation-AlgebraicEquation-1"
];


VerificationTest[
  model = SIRModel[t, "TotalPopulationRepresentation" -> "SumSubstitution", "InitialConditions" -> True, "RateRules" -> True ];
  LongModelQ[model] &&
      ! MemberQ[model["InitialConditions"][[All, 1]], Keys[Select[model["Stocks"], # == "Total Population" &]][[1]]] &&
      MemberQ[Keys[model["RateRules"]], Keys[Select[model["Stocks"], # == "Total Population" &]][[1]]]
  ,
  True
  ,
  TestID -> "SIRModel-TotalPopulationRepresentation-SumSubstitution-1"
];


(***********************************************************)
(* Generate SI2R model                                     *)
(***********************************************************)


(***********************************************************)
(* Generate SEI2R model                                    *)
(***********************************************************)



(***********************************************************)
(* Model grid table-form                                   *)
(***********************************************************)


EndTestSection[]
