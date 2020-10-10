(*
    Epidemiology models Mathematica package
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

(* :Title: EpidemiologyModels *)
(* :Context: EpidemiologyModels` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Epidemiology, model, SIR, System dynamics *)
(* :Discussion:

   # In brief

   The main purpose of this package is to provide equations and related stocks and rates dictionaries
   for well known and specific epidemiological models.

   # Usage

   This command generates the SIR model in the context "MyContext`":

      SIRModel[t, "Global`MyContext`"]


*)


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["EpidemiologyModels`"];
(* Exported symbols added here with SymbolName::usage *)

EpidemiologyModelQ::usage = "Is the argument an association with stocks, rates, and equations?";

EpidemiologyFullModelQ::usage = "Is the argument an association with \
stocks, rates, equations, initial conditions, and rate rules ?";

MalariaModel::usage = "MalariaModel[var, con] generates malaria model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SIRModel::usage = "SIRModel[var, con] generates SIR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SI2RModel::usage = "SI2RModel[var, con] generates SI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEIRModel::usage = "SEIRModel[var, con] generates SEIR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2RModel::usage = "SEI2RModel[var, con] generates SEI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2HRModel::usage = "SEI2HRModel[var, con] generates hospitalization SEI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2HREconModel::usage = "SEI2HREconModel[var, con] generates economics SEI2HR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI4RModel::usage = "SEI4RModel[var, con] generates SEI4R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

ModelGridTableForm::usage = "Displays the model legibly.";

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

Clear[EpidemiologyFullModelQ];
EpidemiologyFullModelQ[model_] :=
    AssociationQ[model] &&
        Sort[Keys[model]] == Sort[{ "Stocks", "Rates", "Equations", "InitialConditions", "RateRules" }] &&
        AssociationQ[model["Stocks"]] &&
        AssociationQ[model["Rates"]] &&
        MatchQ[model["Equations"], { _Equal .. }] &&
        AssociationQ[model["RateRules"]] &&
        MatchQ[model["InitialConditions"], { _Equal .. }];


(***********************************************************)
(* RossMalaria                                             *)
(***********************************************************)

Clear[MalariaModel];

SyntaxInformation[MalariaModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

MalariaModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

MalariaModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[MalariaModel] = { "TotalPopulationRepresentation" -> "Constant", "InitialConditions" -> True, "RateRules" -> True, "Ross" -> False };

MalariaModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, rossQ, tpRepr,
      aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions,
      newlyInfectedHumansTerm, newlyInfectedMosquitoesTerm},

      addInitialConditionsQ = TrueQ[ OptionValue[ MalariaModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ MalariaModel, "RateRules" ] ];

      rossQ = TrueQ[ OptionValue[ MalariaModel, "Ross" ] ];

      tpRepr = OptionValue[ MalariaModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[MalariaModel::"ntpval"];
        $Failed
      ];

      With[{
        HP = ToExpression[ context <> "HP"],
        MP = ToExpression[ context <> "MP"],
        SHP = ToExpression[ context <> "SHP"],
        SMP = ToExpression[ context <> "SMP"],
        IHP = ToExpression[ context <> "IHP"],
        IMP = ToExpression[ context <> "IMP"],
        a = ToExpression[ context <> "a"],
        b = ToExpression[ context <> "b"],
        c = ToExpression[ context <> "c"],
        m = ToExpression[ context <> "m"],
        mls = ToExpression[ context <> "mls"],
        aip = ToExpression[ context <> "aip"]
      },

        (* Stocks *)
        aStocks =
            <|
              HP[t] -> "Human Population",
              MP[t] -> "Mosquito Population",
              SHP[t] -> "Susceptible Human Population",
              SMP[t] -> "Susceptible Mosquito Population",
              IHP[t] -> "Infected Human Population",
              IMP[t] -> "Infected Mosquito Population"
            |>;

        (* Rates  *)
        aRates =
            <|
              a -> "Number of bites per mosquito and time unit",
              b -> "Probability that a bite generates a human infection",
              c -> "Probability that a mosquito becomes infected",
              (* c -> "Proportion of bites by which one susceptible mosquito becomes infected", *)
              (* m -> "Ratio of the number of female mosquitoes to that of humans",*)
              aip -> "Average human infectious period",
              mls -> "Mosquito life span"
            |>;

        (* Equations  *)
        If[ rossQ,
          (* Ross definition *)
          lsEquations = {
            IHP'[t] == a * b * ( IMP[t] / HP[0] ) * ( HP[0] - IHP[t] ) - IHP[t] / aip,
            IMP'[t] == a * c * ( IHP[t] / MP[0] ) * ( MP[0] - IMP[t] ) - IMP[t] / mls
          },

          (* ELSE *)

          newlyInfectedHumansTerm = a * b * ( IMP[t] / HP[t] ) * ( HP[t] - IHP[t] );

          newlyInfectedMosquitoesTerm = a * c * ( IHP[t] / MP[t] ) * ( MP[t] - IMP[t] );

          lsEquations = {
            SHP'[t] == - newlyInfectedHumansTerm,
            IHP'[t] == newlyInfectedHumansTerm - IHP[t] / aip,
            SMP'[t] == - newlyInfectedMosquitoesTerm,
            IMP'[t] == newlyInfectedMosquitoesTerm - IMP[t] / mls
          };

          Which[
            MemberQ[{Constant, "Constant"}, tpRepr],
            lsEquations = lsEquations /. { MP[t] -> MP[0], HP[t] -> HP[0] },

            tpRepr == "SumSubstitution",
            lsEquations = lsEquations /. { HP[t] -> ( SHP[t] + IHP[t] ), MP[t] -> ( SMP[t] + IMP[t] ) },

            tpRepr == "AlgebraicEquation",
            lsEquations = Join[lsEquations, { HP[t] == Max[ 0, SHP[t] + IHP[t] ], MP[t] == Max[ 0, SMP[t] + IMP[t] ] } ]
          ]

        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <|
              HP[0] -> 100000,
              MP[0] -> 1000000,
              a -> 0.5,
              b -> 0.35,
              c -> 0.5,
              aip -> 20,
              mls -> 20
            |>;

        (* Initial conditions *)
        If[ rossQ,
          aInitialConditions =
              {
                IHP[0] == 1,
                IMP[0] == 1
              },
          (* ELSE *)

          aInitialConditions =
              {
                SHP[0] == (HP[0] /. aRateRules) - 1,
                IHP[0] == 1,
                SMP[0] == (MP[0] /. aRateRules) - 1,
                IMP[0] == 1
              }
        ];

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[0]]
        ];

        If[ addRateRulesQ,
          aRes = Append[aRes, "RateRules" -> aRateRules]
        ];

        If[ addInitialConditionsQ,
          aRes = Append[aRes, "InitialConditions" -> aInitialConditions];
        ];

        aRes
      ]
    ];

MalariaModel[___] :=
    Block[{},
      Message[MalariaModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SI2R                                                    *)
(***********************************************************)

Clear[SIRModel];

SyntaxInformation[SIRModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SIRModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SIRModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SIRModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> True, "RateRules" -> True };

SIRModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, tpRepr,
      newlyInfectedTerm, aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SIRModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SIRModel, "RateRules" ] ];

      tpRepr = OptionValue[ SIRModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SIRModel::"ntpval"];
        $Failed
      ];

      With[{
        TP = ToExpression[ context <> "TP"],
        SP = ToExpression[ context <> "SP"],
        IP = ToExpression[ context <> "IP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        lpcr = ToExpression[ context <> "lpcr"]
      },

        (* Stocks *)
        aStocks =
            <|TP[t] -> "Total Population" ,
              SP[t] -> "Susceptible Population",
              IP[t] -> "Infected Population",
              RP[t] -> "Recovered Population",
              MLP[t] -> "Money of Lost Productivity"|>;

        (* Rates  *)
        aRates =
            <|
              deathRate[TP] -> "Population death rate",
              deathRate[IP] -> "Infected Population death rate",
              contactRate[IP] -> "Contact rate for the infected population",
              aip -> "Average infectious period",
              lpcr[IP] -> "Lost productivity cost rate (per person per day)"
            |>;

        (* Equations  *)
        newlyInfectedTerm = contactRate[IP] / TP[t] * SP[t] * IP[t];

        lsEquations = {
          SP'[t] == -newlyInfectedTerm - deathRate[TP] * SP[t],
          IP'[t] == newlyInfectedTerm - (1 / aip) * IP[t] - deathRate[IP] * IP[t],
          RP'[t] == (1 / aip) * IP[t] - deathRate[TP] * RP[t],
          MLP'[t] == lpcr[IP] * (TP[t] - RP[t] - SP[t])
        };

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsEquations = lsEquations /. TP[t] -> TP[0],

          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + IP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == Max[ 0, SP[t] + IP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TP[0] -> 100000,
              deathRate[TP] -> (800 / 10^5) / 365,
              deathRate[IP] -> 0.035 / aip,
              contactRate[IP] -> 0.15,
              aip -> 4 * 7,
              lpcr[IP] -> 1
            |>;

        (* Initial conditions *)
        aInitialConditions =
            {
              SP[0] == (TP[0] /. aRateRules) - 1,
              IP[0] == 1,
              RP[0] == 0,
              MLP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[0]]
        ];

        If[ addRateRulesQ,
          aRes = Append[aRes, "RateRules" -> aRateRules]
        ];

        If[ addInitialConditionsQ,
          aRes = Append[aRes, "InitialConditions" -> aInitialConditions];
        ];

        aRes
      ]
    ];

SIRModel[___] :=
    Block[{},
      Message[SIRModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SI2R                                                    *)
(***********************************************************)

Clear[SI2RModel];

SyntaxInformation[SI2RModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SI2RModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SI2RModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SI2RModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> True, "RateRules" -> True, "BirthsTerm" -> False };

SI2RModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, birthsTermQ, tpRepr,
      newlyInfectedTerm, aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SI2RModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SI2RModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[ SI2RModel, "BirthsTerm" ] ];

      tpRepr = OptionValue[ SI2RModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SIRModel::"ntpval"];
        $Failed
      ];

      With[{
        TP = ToExpression[ context <> "TP"],
        SP = ToExpression[ context <> "SP"],
        INSP = ToExpression[ context <> "INSP"],
        ISSP = ToExpression[ context <> "ISSP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        sspf = ToExpression[ context <> "sspf"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        lpcr = ToExpression[ context <> "lpcr"]
      },

        (* Stocks *)
        aStocks =
            <|TP[t] -> "Total Population" ,
              SP[t] -> "Susceptible Population",
              INSP[t] -> "Infected Normally Symptomatic Population",
              ISSP[t] -> "Infected Severely Symptomatic Population",
              RP[t] -> "Recovered Population",
              MLP[t] -> "Money of Lost Productivity"|>;

        (* Rates  *)
        aRates =
            <|
              deathRate[TP] -> "Population death rate",
              deathRate[INSP] -> "Infected Normally Symptomatic Population death rate",
              deathRate[ISSP] -> "Infected Severely Symptomatic Population death rate",
              sspf[SP] -> "Severely Symptomatic Population Fraction" ,
              contactRate[INSP] -> "Contact rate for the infected normally symptomatic population",
              contactRate[ISSP] -> "Contact rate for the infected severely symptomatic population",
              aip -> "Average infectious period",
              lpcr[ISSP, INSP] -> "Lost productivity cost rate (per person per day)"
            |>;

        (* Equations  *)
        newlyInfectedTerm = contactRate[ISSP] / TP[t] * SP[t] * ISSP[t] + contactRate[INSP] / TP[t] * SP[t] * INSP[t];

        lsEquations = {

          If[ birthsTermQ,
            SP'[t] == deathRate[TP] * TP[t] - newlyInfectedTerm - deathRate[TP] * SP[t],
            (* ELSE *)
            SP'[t] == - newlyInfectedTerm - deathRate[TP] * SP[t]
          ],
          INSP'[t] == (1 - sspf[SP]) * newlyInfectedTerm - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * newlyInfectedTerm - (1 / aip) * ISSP[t] - deathRate[ISSP] * ISSP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          MLP'[t] == lpcr[ISSP, INSP] * (TP[t] - RP[t] - SP[t])
        };

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsEquations = lsEquations /. TP[t] -> TP[0],

          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + INSP[t] + ISSP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == Max[ 0, SP[t] + INSP[t] + ISSP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TP[0] -> 100000,
              deathRate[TP] -> (800 / 10^5) / 365,
              deathRate[ISSP] -> 0.035 / aip,
              deathRate[INSP] -> 0.01 / aip,
              contactRate[ISSP] -> 0.15,
              contactRate[INSP] -> 0.15,
              aip -> 26,
              sspf[SP] -> 0.2,
              lpcr[ISSP, INSP] -> 1
            |>;

        (* Initial conditions *)
        aInitialConditions =
            {
              SP[0] == (TP[0] /. aRateRules) - 2,
              ISSP[0] == 1,
              INSP[0] == 1,
              RP[0] == 0,
              MLP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[0]]
        ];

        If[ addRateRulesQ,
          aRes = Append[aRes, "RateRules" -> aRateRules]
        ];

        If[ addInitialConditionsQ,
          aRes = Append[aRes, "InitialConditions" -> aInitialConditions];
        ];

        aRes
      ]
    ];

SI2RModel[___] :=
    Block[{},
      Message[SI2RModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SEIR                                                   *)
(***********************************************************)
(*
   Initially I "programmed" this model by just modifying the full SEI2R code.
   But it is better the SEIR implementation to be done with (self contained) model modification.
   The "SEIR as modified SEI2R" is what is implemented. To verify used the comparison:

     Merge[{model2SEIR, modelSEIR}, If[AssociationQ[#[[1]]], Complement @@ Map[Normal, #], Complement @@ #] &]

   with the code modified SEI2R.
*)

Clear[SEIRModel];

SyntaxInformation[SEIRModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEIRModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEIRModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEIRModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> True, "RateRules" -> True, "BirthsTerm" -> False };

SEIRModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addRateRulesQ, addInitialConditionsQ, birthsTermQ, tpRepr,
      model, lsRepRules, lsDropSymbols, m2},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEIRModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEIRModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEIRModel, "BirthsTerm"] ];

      tpRepr = OptionValue[ SEIRModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEIRModel::"ntpval"];
        $Failed
      ];

      With[{
        SP = ToExpression[ context <> "SP"],
        INSP = ToExpression[ context <> "INSP"],
        ISSP = ToExpression[ context <> "ISSP"],
        IP = ToExpression[ context <> "IP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        aincp = ToExpression[ context <> "aincp"],
        lpcr = ToExpression[ context <> "lpcr"],
        sspf = ToExpression[ context <> "sspf"]
      },

        (*
        Below we modify the SEI2R model by essentially zeroing sspf[SP] and ISSP[t].
        Additional corrections of descriptions and and equations were applied.
        *)

        model = SEI2RModel[ t, context, FilterRules[ Join[ {"InitialConditions" -> True, "RateRules" -> True}, {opts}], Options[SEIRModel] ] ];

        lsRepRules = {
          sspf[SP] -> 0,
          ISSP[t] -> 0, INSP[t] -> IP[t], INSP[0] -> IP[0],
          INSP'[t] -> IP'[t], contactRate[INSP] -> contactRate[IP], deathRate[INSP] -> deathRate[IP],
          lpcr[ISSP, INSP] -> lpcr[IP]
        };

        lsDropSymbols = {sspf[SP], contactRate[ISSP], deathRate[ISSP], ISSP[0] == 1};

        m2 = model;

        m2["Stocks"] = KeyDrop[m2["Stocks"], ISSP[t]];
        m2["Stocks"] = KeyMap[# /. lsRepRules &, m2["Stocks"]];
        m2["Stocks"] = Join[m2["Stocks"], <|IP[t] -> "Infected Population"|>];

        m2["Rates"] = KeyDrop[m2["Rates"], lsDropSymbols];
        m2["Rates"] = KeyMap[# /. lsRepRules &, m2["Rates"]];
        m2["Rates"] =
            Join[
              m2["Rates"],
              <|
                deathRate[IP] -> "Infected Population death rate",
                contactRate[IP] -> "Contact rate for the infected population"
              |>
            ];

        m2["RateRules"] = KeyDrop[m2["RateRules"], lsDropSymbols];
        m2["Equations"] = DeleteCases[m2["Equations"], Equal[ISSP'[t], __]];
        m2["Equations"] = Map[# /. lsRepRules &, m2["Equations"]];
        m2["InitialConditions"] = DeleteCases[m2["InitialConditions"], ISSP[0] == 1] /. lsRepRules;
        m2["RateRules"] = Association[Normal[m2["RateRules"]] /. lsRepRules];

        m2
      ]
    ];

SEIRModel[___] :=
    Block[{},
      Message[SEIRModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SEI2R                                                   *)
(***********************************************************)

Clear[SEI2RModel];

SyntaxInformation[SEI2RModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEI2RModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEI2RModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEI2RModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> True, "RateRules" -> True, "BirthsTerm" -> False };

SEI2RModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addRateRulesQ, addInitialConditionsQ, birthsTermQ, tpRepr,
      newlyInfectedTerm, aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEI2RModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEI2RModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEI2RModel, "BirthsTerm"] ];

      tpRepr = OptionValue[ SEI2RModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SIRModel::"ntpval"];
        $Failed
      ];

      With[{
        TP = ToExpression[ context <> "TP"],
        SP = ToExpression[ context <> "SP"],
        EP = ToExpression[ context <> "EP"],
        INSP = ToExpression[ context <> "INSP"],
        ISSP = ToExpression[ context <> "ISSP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        sspf = ToExpression[ context <> "sspf"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        aincp = ToExpression[ context <> "aincp"],
        lpcr = ToExpression[ context <> "lpcr"]
      },

        (* Stocks *)
        aStocks =
            <|TP[t] -> "Total Population" ,
              SP[t] -> "Susceptible Population",
              EP[t] -> "Exposed Population",
              INSP[t] -> "Infected Normally Symptomatic Population",
              ISSP[t] -> "Infected Severely Symptomatic Population",
              RP[t] -> "Recovered Population",
              MLP[t] -> "Money of Lost Productivity"|>;

        (* Rates  *)
        aRates =
            <|
              deathRate[TP] -> "Population death rate",
              deathRate[INSP] -> "Infected Normally Symptomatic Population death rate",
              deathRate[ISSP] -> "Infected Severely Symptomatic Population death rate",
              sspf[SP] -> "Severely Symptomatic Population Fraction" ,
              contactRate[INSP] -> "Contact rate for the infected normally symptomatic population",
              contactRate[ISSP] -> "Contact rate for the infected severely symptomatic population",
              aip -> "Average infectious period",
              aincp -> "Average incubation period",
              lpcr[ISSP, INSP] -> "Lost productivity cost rate (per person per day)"
            |>;

        (* Equations  *)
        newlyInfectedTerm = contactRate[ISSP] / TP[t] * SP[t] * ISSP[t] + contactRate[INSP] / TP[t] * SP[t] * INSP[t];

        lsEquations = {
          If[ birthsTermQ,
            SP'[t] == deathRate[TP] * TP[t] - newlyInfectedTerm - deathRate[TP] * SP[t],
            (* ELSE *)
            SP'[t] == - newlyInfectedTerm - deathRate[TP] * SP[t]
          ],
          EP'[t] == newlyInfectedTerm - (deathRate[TP] + (1 / aincp) ) * EP[t],
          INSP'[t] == (1 - sspf[SP]) * (1 / aincp) * EP[t] - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * (1 / aincp) * EP[t] - (1 / aip) * ISSP[t] - deathRate[ISSP] * ISSP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          MLP'[t] == lpcr[ISSP, INSP] * (TP[t] - RP[t] - SP[t])
        };

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsEquations = lsEquations /. TP[t] -> TP[0],

          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == Max[ 0, SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TP[0] -> 100000,
              deathRate[TP] -> (800 / 10^5) / 365,
              deathRate[ISSP] -> 0.035 / aip,
              deathRate[INSP] -> 0.01 / aip,
              contactRate[ISSP] -> 0.15,
              contactRate[INSP] -> 0.15,
              aip -> 26,
              aincp -> 6,
              sspf[SP] -> 0.2,
              lpcr[ISSP, INSP] -> 1
            |>;

        (* Initial conditions *)
        aInitialConditions = {
          SP[0] == (TP[0] /. aRateRules) - 2,
          EP[0] == 0,
          ISSP[0] == 1,
          INSP[0] == 1,
          RP[0] == 0,
          MLP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[0]]
        ];

        If[ addRateRulesQ,
          aRes = Append[aRes, "RateRules" -> aRateRules]
        ];

        If[ addInitialConditionsQ,
          aRes = Append[aRes, "InitialConditions" -> aInitialConditions];
        ];

        aRes
      ]
    ];

SEI2RModel[___] :=
    Block[{},
      Message[SEI2RModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SEI2HRModel                                             *)
(***********************************************************)

Clear[SEI2HRModel];

SyntaxInformation[SEI2HRModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEI2HRModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEI2HRModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";


Options[SEI2HRModel] = Options[SEI2RModel];

SEI2HRModel[ t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, birthsTermQ, tpRepr,
      aNewStocks, aNewRates, lsNewEquations, model, newModel,
      newBySeverelyInfectedTerm, newByNormallyInfectedTerm, newlyInfectedTerm, peopleDyingPerDay, pos},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEI2HRModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEI2HRModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEI2HRModel, "BirthsTerm"] ];

      tpRepr = OptionValue[ SEI2HRModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEI2HRModel::"ntpval"];
        $Failed
      ];

      model = SEI2RModel[ t, context, FilterRules[ Join[ {"InitialConditions" -> True, "RateRules" -> True}, {opts}], Options[SEI2RModel] ] ];

      newModel = model;

      With[{
        (* standard *)
        TP = ToExpression[ context <> "TP"],
        SP = ToExpression[ context <> "SP"],
        EP = ToExpression[ context <> "EP"],
        INSP = ToExpression[ context <> "INSP"],
        ISSP = ToExpression[ context <> "ISSP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        sspf = ToExpression[ context <> "sspf"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        aincp = ToExpression[ context <> "aincp"],
        lpcr = ToExpression[ context <> "lpcr"],
        (* new *)
        HP = ToExpression[ context <> "HP"],
        DIP = ToExpression[ context <> "DIP"],
        HB = ToExpression[ context <> "HB"],
        MHS = ToExpression[ context <> "MHS"],
        nhbr = ToExpression[ context <> "nhbr"],
        hscr = ToExpression[ context <> "hscr"],
        nhbcr = ToExpression[ context <> "nhbcr"]
      },

        (* Stocks *)
        aNewStocks = <|
          HP[t] -> "Hospitalized Population",
          DIP[t] -> "Deceased Infected Population",
          HB[t] -> "Hospital Beds",
          MHS[t] -> "Money for Hospital Services"|>;

        newModel["Stocks"] = Join[ newModel["Stocks"], aNewStocks ];

        (* New Rates *)
        aNewRates = <|
          deathRate[HP] -> "Hospitalized Population death rate",
          contactRate[HP] -> "Contact rate for the hospitalized population",
          nhbr[TP] -> "Number of hospital beds rate",
          hscr[ISSP, INSP] -> "Hospital services cost rate (per bed per day)",
          nhbcr[ISSP, INSP] -> "Number of hospital beds change rate (per day)"
        |>;

        newModel["Rates"] = Join[ newModel["Rates"], aNewRates ];

        (* New and changed Equations *)

        newBySeverelyInfectedTerm = contactRate[ISSP] / TP[t] * SP[t] * Max[ISSP[t] - HP[t], 0] + contactRate[HP] / TP[t] * SP[t] * HP[t];
        newByNormallyInfectedTerm = contactRate[INSP] / TP[t] * SP[t] * INSP[t];
        newlyInfectedTerm = newBySeverelyInfectedTerm + newByNormallyInfectedTerm;

        peopleDyingPerDay = deathRate[ISSP] * ( ISSP[t] - HP[t] ) + deathRate[INSP] * INSP[t] + deathRate[HP] * HP[t];

        lsNewEquations = {
          If[ birthsTermQ,
            SP'[t] == deathRate[TP] * TP[t] - newlyInfectedTerm - deathRate[TP] * SP[t],
            (* ELSE *)
            SP'[t] == - newlyInfectedTerm - deathRate[TP] * SP[t]
          ],
          EP'[t] == newlyInfectedTerm - (deathRate[TP] + (1 / aincp) ) * EP[t],
          INSP'[t] == (1 - sspf[SP]) * (1 / aincp) * EP[t] - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * (1 / aincp) * EP[t] - (1 / aip) * ISSP[t] - deathRate[ISSP] * ( ISSP[t] - HP[t] ) - deathRate[HP] * HP[t],
          HP'[t] == Piecewise[{{Min[HB[t] - HP[t], sspf[SP] * (1 / aincp) * EP[t]], HP[t] < HB[t]}}, 0] - (1 / aip) * HP[t] - deathRate[HP] * HP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          DIP'[t] == peopleDyingPerDay,
          HB'[t] == nhbcr[ISSP, INSP] * HB[t],
          MHS'[t] == hscr[ISSP, INSP] * HP[t],
          MLP'[t] == lpcr[ISSP, INSP] * (ISSP[t] + INSP[t] + peopleDyingPerDay)
        };

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsNewEquations = lsNewEquations /. TP[t] -> TP[0],

          tpRepr == "SumSubstitution",
          lsNewEquations = lsNewEquations /. TP[t] -> ( SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] )
        ];

        pos = Position[ model["Equations"], #]& /@ lsNewEquations[[All, 1]];
        pos = Flatten @ Map[ If[ Length[#] == 0, {}, First @ Flatten @ # ]&, pos ];
        newModel["Equations"] = Join[ Delete[newModel["Equations"], List /@ pos], lsNewEquations ];

        (* New Initial conditions *)
        If[ addInitialConditionsQ,
          newModel["InitialConditions"] =
              Join[
                newModel["InitialConditions"],
                {
                  HP[0] == 0,
                  DIP[0] == 0,
                  HB[0] == nhbr[TP] * (TP[0] /. newModel["RateRules"]),
                  MHS[0] == 0}
              ];
        ];

        (* New Rate Rules *)
        If[ addRateRulesQ,
          newModel["RateRules"] =
              Join[
                newModel["RateRules"],
                <|
                  deathRate[HP] -> 0.25 * deathRate[ISSP],
                  contactRate[HP] -> 0.1 * contactRate[ISSP],
                  nhbr[TP] -> 2.9 / 1000,
                  nhbcr[ISSP, INSP] -> 0,
                  hscr[ISSP, INSP] -> 1|>
              ];
        ];

        newModel
      ]
    ];

SEI2HRModel[___] :=
    Block[{},
      Message[SEI2HRModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SEI2HREconModel                                         *)
(***********************************************************)

Clear[SEI2HREconModel];

SyntaxInformation[SEI2HREconModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEI2HREconModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEI2HREconModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEI2HREconModel] = Options[SEI2RModel];

SEI2HREconModel[ t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, birthsTermQ, tpRepr,
      aNewStocks, aNewRates, lsNewEquations, model, newModel,
      newBySeverelyInfectedTerm, newByNormallyInfectedTerm, newlyInfectedTerm, peopleDyingPerDay,
      usableHospitalBeds, orderedHospitalSupplies, pos},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEI2HREconModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEI2HREconModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEI2HREconModel, "BirthsTerm"] ];

      tpRepr = OptionValue[ SEI2HREconModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEI2HREconModel::"ntpval"];
        $Failed
      ];

      model = SEI2RModel[ t, context, FilterRules[ Join[ {"InitialConditions" -> True, "RateRules" -> True}, {opts}], Options[SEI2RModel] ] ];

      newModel = model;

      With[{
        (* standard *)
        TP = ToExpression[ context <> "TP"],
        SP = ToExpression[ context <> "SP"],
        EP = ToExpression[ context <> "EP"],
        INSP = ToExpression[ context <> "INSP"],
        ISSP = ToExpression[ context <> "ISSP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        sspf = ToExpression[ context <> "sspf"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        aincp = ToExpression[ context <> "aincp"],
        lpcr = ToExpression[ context <> "lpcr"],
        capacity = ToExpression[ context <> "\[Kappa]"],
        (* new *)
        HP = ToExpression[ context <> "HP"],
        DIP = ToExpression[ context <> "DIP"],
        HB = ToExpression[ context <> "HB"],
        MSD = ToExpression[ context <> "MSD"],
        MMSP = ToExpression[ context <> "MMSP"],
        MHS = ToExpression[ context <> "MHS"],
        MS = ToExpression[ context <> "MS"],
        HMS = ToExpression[ context <> "HMS"],
        nhbr = ToExpression[ context <> "nhbr"],
        hscr = ToExpression[ context <> "hscr"],
        nhbcr = ToExpression[ context <> "nhbcr"],
        hpmscr = ToExpression[ context <> "hpmscr"],
        upmscr = ToExpression[ context <> "upmscr"],
        mspr = ToExpression[ context <> "mspr"],
        mspcr = ToExpression[ context <> "mspcr"],
        mscr = ToExpression[ context <> "mscr"],
        msdr = ToExpression[ context <> "msdr"],
        msdp = ToExpression[ context <> "msdp"]
      },

        (* Stocks *)
        aNewStocks = <|
          HP[t] -> "Hospitalized Population",
          DIP[t] -> "Deceased Infected Population",
          MS[t] -> "Medical Supplies",
          MSD[t] -> "Medical Supplies Demand",
          HB[t] -> "Hospital Beds",
          MMSP[t] -> "Money for Medical Supplies Production",
          MHS[t] -> "Money for Hospital Services",
          HMS[t] -> "Hospital Medical Supplies"|>;

        newModel["Stocks"] = Join[ newModel["Stocks"], aNewStocks ];

        (* New Rates *)
        aNewRates = <|
          deathRate[HP] -> "Hospitalized Population death rate",
          contactRate[HP] -> "Contact rate for the hospitalized population",
          nhbr[TP] -> "Number of hospital beds rate (number of beds per person)",
          hscr[ISSP, INSP] -> "Hospital services cost rate (per bed per day)",
          nhbcr[ISSP, INSP] -> "Number of hospital beds change rate (per day)",
          contactRate[HP] -> "Contact rate for the hospitalized population",
          hpmscr[ISSP, INSP] -> "Hospitalized population medical supplies consumption rate (per day)",
          upmscr[ISSP, INSP] -> "Un-hospitalized population medical supplies consumption rate (units per day)",
          mspr[HB] -> "Medical supplies production rate (units per pay)",
          mspcr[HB] -> "Medical supplies production cost rate (per unit)",
          msdr[HB] -> "Medical supplies delivery rate (delay factor)",
          msdp[HB] -> "Medical supplies delivery period (number of days)",
          mscr[TP] -> "Medical supplies consumption rate (units per day per person)",
          mscr[INSP] -> "Medical supplies consumption rate (units per day per person)",
          mscr[ISSP] -> "Medical supplies consumption rate (units per day per person)",
          mscr[HP] -> "Medical supplies consumption rate (units per day per person)",
          capacity[HMS] -> "Capacity to store Hospital Medical Supplies",
          capacity[MS] -> "Capacity to store produced Medical Supplies",
          capacity[MSD] -> "Capacity to transport produced Medical Supplies"
        |>;

        newModel["Rates"] = Join[ newModel["Rates"], aNewRates ];

        (* New and changed Equations *)

        newBySeverelyInfectedTerm = contactRate[ISSP] / TP[t] * SP[t] * Max[ISSP[t] - HP[t], 0] + contactRate[HP] / TP[t] * SP[t] * HP[t];
        newByNormallyInfectedTerm = contactRate[INSP] / TP[t] * SP[t] * INSP[t];
        newlyInfectedTerm = newBySeverelyInfectedTerm + newByNormallyInfectedTerm;

        usableHospitalBeds = Min[HB[t] - HP[t], HMS[t] / mscr[ISSP]];

        peopleDyingPerDay = deathRate[ISSP] * ( ISSP[t] - HP[t] ) + deathRate[INSP] * INSP[t] + deathRate[HP] * HP[t];

        orderedHospitalSupplies = Min[ capacity[MSD], MS[t], mscr[HP] * HB[t], capacity[HMS] - HMS[t] ] / msdp[HB];

        lsNewEquations = {
          If[ birthsTermQ,
            SP'[t] == deathRate[TP] * TP[t] - newlyInfectedTerm - deathRate[TP] * SP[t],
            (* ELSE *)
            SP'[t] == - newlyInfectedTerm - deathRate[TP] * SP[t]
          ],
          EP'[t] == newlyInfectedTerm - (deathRate[TP] + (1 / aincp)) * EP[t],
          INSP'[t] == (1 - sspf[SP]) * (1 / aincp) * EP[t] - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * (1 / aincp) * EP[t] - (1 / aip) * ISSP[t] - deathRate[ISSP] * ( ISSP[t] - HP[t] ) - deathRate[HP] * HP[t],
          HP'[t] == Piecewise[{{Min[usableHospitalBeds, sspf[SP] * (1 / aincp) * EP[t]], HP[t] < HB[t]}}, 0] - (1 / aip) * HP[t] - deathRate[HP] * HP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          DIP'[t] == peopleDyingPerDay,
          HB'[t] == nhbcr[ISSP, INSP] * HB[t],
          HMS'[t] == -Min[HMS[t], mscr[ISSP] * HP[t]] + orderedHospitalSupplies,
          MS'[t] == Min[ mspr[HB], capacity[MS] - MS[t] ] - orderedHospitalSupplies - Min[ MS[t] - orderedHospitalSupplies, mscr[ISSP] * (ISSP[t] - HP[t]) + mscr[INSP] * INSP[t] + mscr[TP] * (SP[t] + EP[t] + RP[t]) ],
          MSD'[t] == mscr[HP] * HP[t] + mscr[ISSP] * ISSP[t] + mscr[INSP] * INSP[t] + mscr[TP] * (SP[t] + EP[t] + RP[t]),
          MHS'[t] == hscr[ISSP, INSP] * HP[t],
          MMSP'[t] == mspcr[HB] * MSD[t],
          MLP'[t] == lpcr[ISSP, INSP] * (ISSP[t] + INSP[t] + peopleDyingPerDay)
        };

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsNewEquations = lsNewEquations /. TP[t] -> TP[0],

          tpRepr == "SumSubstitution",
          lsNewEquations = lsNewEquations /. TP[t] -> ( SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] )
        ];

        pos = Position[ model["Equations"], #]& /@ lsNewEquations[[All, 1]];
        pos = Flatten @ Map[ If[ Length[#] == 0, {}, First @ Flatten @ # ]&, pos ];
        newModel["Equations"] = Join[ Delete[newModel["Equations"], List /@ pos], lsNewEquations ];

        (* New Initial conditions *)
        If[ addInitialConditionsQ,

          (*
                    eqMSD0 = Cases[newModel["Equations"], MSD[t] == _ ][[1]];
                    eqMSD0 = eqMSD0 /. t -> 0 ;
                    eqMSD0 = eqMSD0[[1]] == ( eqMSD0[[2]] /. Association[ Rule @@@ newModel["InitialConditions"] ] );
          *)

          newModel["InitialConditions"] =
              Join[
                newModel["InitialConditions"],
                {
                  HP[0] == 0,
                  DIP[0] == 0,
                  HB[0] == nhbr[TP] * (TP[0] /. newModel["RateRules"]),
                  MSD[0] == 0,
                  MHS[0] == 0,
                  MMSP[0] == 0,
                  HMS[0] == (capacity[HMS] //. Prepend[ newModel["RateRules"], HB[0] -> (nhbr[TP] * TP[0] /. newModel["RateRules"]) ] ),
                  MS[0] == (capacity[MS] //. newModel["RateRules"])
                }
              ];
        ];

        (* New Rate Rules *)
        If[ addRateRulesQ,
          newModel["RateRules"] =
              Join[
                newModel["RateRules"],
                <|
                  deathRate[HP] -> 0.25 * deathRate[ISSP],
                  contactRate[HP] -> 0.1 * contactRate[ISSP],
                  nhbr[TP] -> 2.9 / 1000,
                  nhbcr[ISSP, INSP] -> 0,
                  hscr[ISSP, INSP] -> 1,
                  hpmscr[ISSP, INSP] -> 4,
                  upmscr[ISSP, INSP] -> 2,
                  mspcr[HB] -> 1,
                  msdp[HB] -> 1.5,
                  mspr[HB] -> 100 * nhbr[TP] * TP[0],
                  mscr[TP] -> 0.02,
                  mscr[INSP] -> 0.7,
                  mscr[ISSP] -> 3,
                  mscr[HP] -> 4,
                  capacity[HMS] -> HB[0] * mscr[ISSP] * 2,
                  capacity[MS] -> TP[0] * mscr[TP] * 2,
                  capacity[MSD] -> HB[0] / 10
                |>
              ];
        ];

        newModel
      ]
    ];

SEI2HREconModel[___] :=
    Block[{},
      Message[SEI2HREconModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* SEI4RModel                                              *)
(***********************************************************)

Clear[SEI4RModel];

SyntaxInformation[SEI4RModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEI4RModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEI4RModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";


Options[SEI4RModel] = Options[SEI2RModel];

SEI4RModel[ t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, birthsTermQ, tpRepr,
      aNewStocks, aNewRates, lsNewEquations, model, newModel,
      newBySeverelyInfectedTerm, newByNormallyInfectedTerm, newlyInfectedTerm, peopleDyingPerDay, pos},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEI4RModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEI4RModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEI4RModel, "BirthsTerm"] ];

      tpRepr = OptionValue[ SEI4RModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEI4RModel::"ntpval"];
        $Failed
      ];

      model = SEI2RModel[ t, context, FilterRules[ Join[ {"InitialConditions" -> True, "RateRules" -> True}, {opts}], Options[SEI2RModel] ] ];

      newModel = model;

      With[{
        (* standard *)
        TP = ToExpression[ context <> "TP"],
        SP = ToExpression[ context <> "SP"],
        EP = ToExpression[ context <> "EP"],
        INSP = ToExpression[ context <> "INSP"],
        ISSP = ToExpression[ context <> "ISSP"],
        RP = ToExpression[ context <> "RP"],
        MLP = ToExpression[ context <> "MLP"],
        deathRate = ToExpression[ context <> "\[Mu]"],
        sspf = ToExpression[ context <> "sspf"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        aincp = ToExpression[ context <> "aincp"],
        lpcr = ToExpression[ context <> "lpcr"],
        (* new *)
        IAP = ToExpression[ context <> "IAP"],
        ICIP = ToExpression[ context <> "ICIP"],
        DIP = ToExpression[ context <> "DIP"],
        nspf = ToExpression[ context <> "nspf" ],
        cipf = ToExpression[ context <> "cipf" ]
      },

        (* Stocks *)
        aNewStocks = <|
          IAP[t] -> "Infected Asymptomatic Population",
          ICIP[t] -> "Infected Critically Ill Population",
          DIP[t] -> "Deceased Infected Population"|>;

        newModel["Stocks"] = Join[ newModel["Stocks"], aNewStocks ];

        (* New Rates *)
        aNewRates = <|
          deathRate[IAP] -> "Infected Asymptomatic Population death rate",
          deathRate[ICIP] -> "Infected Critically Ill Population death rate",
          contactRate[IAP] -> "Contact rate for the infected asymptomatic population",
          contactRate[ICIP] -> "Contact rate for the infected critically ill population",
          nspf[SP] -> "Normally symptomatic population fraction",
          cipf[SP] -> "Critically ill population fraction"
        |>;

        newModel["Rates"] = Join[ newModel["Rates"], aNewRates ];

        (* New and changed Equations *)
        newlyInfectedTerm =
            contactRate[IAP] / TP[t] * SP[t] * IAP[t] +
                contactRate[ISSP] / TP[t] * SP[t] * ISSP[t] +
                contactRate[INSP] / TP[t] * SP[t] * INSP[t] +
                contactRate[ICIP] / TP[t] * SP[t] * ICIP[t];

        peopleDyingPerDay =
            deathRate[IAP] * IAP[t] +
                deathRate[ISSP] * ISSP[t] +
                deathRate[INSP] * INSP[t] +
                deathRate[ICIP] * ICIP[t];

        lsNewEquations = {
          If[ birthsTermQ,
            SP'[t] == deathRate[TP] * TP[t] - newlyInfectedTerm - deathRate[TP] * SP[t],
            (* ELSE *)
            SP'[t] == - newlyInfectedTerm - deathRate[TP] * SP[t]
          ],
          EP'[t] == newlyInfectedTerm - (deathRate[TP] + (1 / aincp) ) * EP[t],
          IAP'[t] == (1 - sspf[SP] - nspf[SP] - cipf[SP]) * (1 / aincp) * EP[t] - (1 / aip) * IAP[t] - deathRate[IAP] * IAP[t],
          INSP'[t] == nspf[SP] * (1 / aincp) * EP[t] - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * (1 / aincp) * EP[t] - (1 / aip) * ISSP[t] - deathRate[ISSP] * ISSP[t],
          ICIP'[t] == cipf[SP] * (1 / aincp) * EP[t] - (1 / aip) * ICIP[t] - deathRate[ICIP] * ICIP[t],
          RP'[t] == (1 / aip) * (IAP[t] + INSP[t] + ISSP[t] + ICIP[t]) - deathRate[TP] * RP[t],
          DIP'[t] == peopleDyingPerDay,
          MLP'[t] == lpcr[ISSP, INSP] * (ISSP[t] + INSP[t] + ICIP[t] + peopleDyingPerDay)
        };

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsNewEquations = lsNewEquations /. TP[t] -> TP[0],

          tpRepr == "SumSubstitution",
          lsNewEquations = lsNewEquations /. TP[t] -> ( SP[t] + EP[t] + IAP[t] + INSP[t] + ISSP[t] + ICIP[t] + RP[t] )
        ];

        pos = Position[ model["Equations"], #]& /@ lsNewEquations[[All, 1]];
        pos = Flatten @ Map[ If[ Length[#] == 0, {}, First @ Flatten @ # ]&, pos ];
        newModel["Equations"] = Join[ Delete[newModel["Equations"], List /@ pos], lsNewEquations ];

        (* New Initial conditions *)
        If[ addInitialConditionsQ,
          newModel["InitialConditions"] =
              Join[
                newModel["InitialConditions"],
                {
                  IAP[0] == 0,
                  ICIP[0] == 0,
                  DIP[0] == 0
                }
              ];
        ];

        (* New Rate Rules *)
        If[ addRateRulesQ,
          newModel["RateRules"] =
              Join[
                newModel["RateRules"],
                <|
                  deathRate[IAP] -> deathRate[INSP],
                  deathRate[ICIP] -> deathRate[ISSP],
                  contactRate[IAP] -> contactRate[INSP],
                  contactRate[ICIP] -> contactRate[ISSP],
                  nspf[SP] -> 0.55,
                  sspf[SP] -> 0.2,
                  cipf[SP] -> 0.05
                |>
              ];
        ];

        newModel
      ]
    ];

SEI4RModel[___] :=
    Block[{},
      Message[SEI4RModel::"nargs"];
      $Failed
    ];


(***********************************************************)
(* ModelGridTableForm                                      *)
(***********************************************************)

Clear[ModelGridTableForm];

SyntaxInformation[ModelGridTableForm] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[ModelGridTableForm] =
    Join[
      { "Tooltips" -> True },
      Options[ResourceFunction["GridTableForm"]],
      Options[Tooltip]
    ];

ModelGridTableForm::"nargs" = "The first argument is expected to be an association; (an epidemiology model).";

ModelGridTableForm[modelArg_?EpidemiologyModelQ, opts : OptionsPattern[] ] :=
    Block[{model = modelArg, tooltipsQ, aTHeadings, aTooltipRules},

      tooltipsQ = TrueQ[ OptionValue[ModelGridTableForm, "Tooltips"] ];

      aTHeadings = <|
        "Stocks" -> {"Symbol", "Description"},
        "Rates" -> {"Symbol", "Description"},
        "Equations" -> {"Equation"},
        "RateRules" -> {"Symbol", "Value"},
        "InitialConditions" -> {"Equation"}
      |>;

      If[tooltipsQ,
        aTooltipRules = KeyValueMap[ #1 -> Tooltip[ #1, #2, FilterRules[{opts}, Options[Tooltip]] ]&, Join[ model["Stocks"], model["Rates"] ] ];

        model["Equations"] = model["Equations"] /. aTooltipRules;

        If[ EpidemiologyFullModelQ[model],
          model["InitialConditions"] = model["InitialConditions"] /. aTooltipRules;
          model["RateRules"] = KeyMap[ # /. aTooltipRules &, model["RateRules"]];
        ]
      ];

      Association @
          KeyValueMap[
            #1 ->
                ResourceFunction["GridTableForm"][
                  If[AssociationQ[#2], List @@@ Normal[#2], List /@ #2],
                  TableHeadings -> aTHeadings[#1],
                  FilterRules[{opts}, Options[ResourceFunction["GridTableForm"]]]
                ]&,
            If[ tooltipsQ,
              model /. aTooltipRules,
              (*ELSE*)
              model
            ]
          ]
    ];

ModelGridTableForm[___] :=
    Block[{},
      Message[ModelGridTableForm::"nargs"];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]

