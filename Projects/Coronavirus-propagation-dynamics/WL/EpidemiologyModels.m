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

SIRModel::usage = "SIRModel[var, con] generates SIR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SI2RModel::usage = "SI2RModel[var, con] generates SI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2RModel::usage = "SEI2RModel[var, con] generates SEI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2HREconModel::usage = "SEI2HREconModel[var, con] generates economics SEI2R model stocks, rates, and equations \
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
(* SI2R                                                    *)
(***********************************************************)

Clear[SIRModel];

SyntaxInformation[SIRModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SIRModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SIRModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SIRModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> False, "RateRules" -> False };

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
          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + IP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == Max[ 0, SP[t] + IP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TP[t] -> 100000,
              deathRate[TP] -> (800 / 10^5) / 365,
              deathRate[IP] -> 0.035 / aip,
              contactRate[IP] -> 6,
              aip -> 4 * 7,
              lpcr[IP] -> 600
            |>;

        (* Initial conditions *)
        aInitialConditions =
            {
              SP[0] == (TP[t] /. aRateRules) - 1,
              IP[0] == 1,
              RP[0] == 0,
              MLP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[t] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[t]]
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

Options[SI2RModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> False, "RateRules" -> False, "BirthsTerm" -> False };

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
              contactRate[INSP] -> "Contact rate for the normally symptomatic population",
              contactRate[ISSP] -> "Contact rate for the severely symptomatic population",
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
          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + INSP[t] + ISSP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == Max[ 0, SP[t] + INSP[t] + ISSP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TP[t] -> 100000,
              deathRate[TP] -> (800 / 10^5) / 365,
              deathRate[ISSP] -> 0.035 / aip,
              deathRate[INSP] -> 0.01 / aip,
              contactRate[ISSP] -> 6,
              contactRate[INSP] -> 3,
              aip -> 4 * 7,
              sspf[SP] -> 0.2,
              lpcr[ISSP, INSP] -> 600
            |>;

        (* Initial conditions *)
        aInitialConditions =
            {
              SP[0] == (TP[t] /. aRateRules) - 2,
              ISSP[0] == 1,
              INSP[0] == 1,
              RP[0] == 0,
              MLP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[t] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[t]]
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
(* SEI2R                                                   *)
(***********************************************************)

Clear[SEI2RModel];

SyntaxInformation[SEI2RModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEI2RModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEI2RModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEI2RModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> False, "RateRules" -> False, "BirthsTerm" -> False };

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
              contactRate[INSP] -> "Contact rate for the normally symptomatic population",
              contactRate[ISSP] -> "Contact rate for the severely symptomatic population",
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
          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == Max[ 0, SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TP[t] -> 100000,
              deathRate[TP] -> (800 / 10^5) / 365,
              deathRate[ISSP] -> 0.035 / aip,
              deathRate[INSP] -> 0.01 / aip,
              contactRate[ISSP] -> 6,
              contactRate[INSP] -> 3,
              aip -> 4 * 7,
              aincp -> 6,
              sspf[SP] -> 0.2,
              lpcr[ISSP, INSP] -> 600
            |>;

        (* Initial conditions *)
        aInitialConditions = {
          SP[0] == (TP[t] /. aRateRules) - 2,
          EP[0] == 0,
          ISSP[0] == 1,
          INSP[0] == 1,
          RP[0] == 0,
          MLP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TP[0] == (TP[t] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TP[t]]
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
(* SEI2HREconModel                                          *)
(***********************************************************)

Clear[SEI2HREconModel];

SyntaxInformation[SEI2HREconModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEI2HREconModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

Options[SEI2HREconModel] = Join[ { "PopulationToHospitalize" -> Automatic }, Options[SEI2RModel] ];

SEI2HREconModel[ t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, birthsTermQ, tpRepr,
      aNewStocks, aNewRates, lsNewEquations, model, newModel,
      newBySeverelyInfectedTerm, newByNormallyInfectedTerm, newlyInfectedTerm, totalNumberOfBedsTerm, pos},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEI2HREconModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEI2HREconModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEI2HREconModel, "BirthsTerm"] ];

      tpRepr = OptionValue[ SEI2HREconModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEI2HREconModel::"ntpval"];
        $Failed
      ];

      model = SEI2RModel[ t, context, FilterRules[ {opts}, Options[SEI2RModel] ] ];

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
        MSD = ToExpression[ context <> "MSD"],
        HBD = ToExpression[ context <> "HBD"],
        H = ToExpression[ context <> "H"],
        MMSP = ToExpression[ context <> "MMSP"],
        MHS = ToExpression[ context <> "MHS"],
        bkh = ToExpression[ context <> "bkh"],
        msdr = ToExpression[ context <> "msdr"],
        nahb = ToExpression[ context <> "nahb"],
        nhbr = ToExpression[ context <> "nhbr"],
        hscr = ToExpression[ context <> "hscr"],
        nhbcr = ToExpression[ context <> "nhbcr"],
        hpmscr = ToExpression[ context <> "hpmscr"],
        upmscr = ToExpression[ context <> "upmscr"],
        mspcr = ToExpression[ context <> "mspcr"]
      },

        (* Stocks *)
        aNewStocks = <|
          H[t] -> "Hospital",
          HP[t] -> "Hospitalized Population",
          DIP[t] -> "Deceased Infected Population",
          MSD[t] -> "Medical Supplies Demand",
          HBD[t] -> "Hospital Beds Demand",
          HB[t] -> "Hospital Beds",
          MMSP[t] -> "Money for Medical Supplies Production",
          MHS[t] -> "Money for Hospital Services" |>;

        newModel["Stocks"] = Join[ newModel["Stocks"], aNewStocks ];

        (* New Rates *)
        aNewRates = <|
          deathRate[HP] -> "Hospitalized Population death rate",
          msdr[MMSP] -> "Medical supplies delivery rate (delay factor)",
          bkh[H] -> "Bed capacity per hospital",
          contactRate[HP] -> "Contact rate for the hospitalized population",
          nahb -> "Number of available hospital beds",
          nhbr[ISSP, INSP] -> "New hospital beds rate",
          hscr[ISSP, INSP] -> "Hospital services cost rate (per bed per day)",
          nhbcr[ISSP, INSP] -> "Number of hospital beds change rate (per day)",
          hpmscr[ISSP, INSP] -> "Hospitalized population medical supplies consumption rate (per day)",
          upmscr[ISSP, INSP] -> "Un-hospitalized population medical supplies consumption rate (units per day)",
          mspcr[ISSP, INSP] -> "Medical supplies production cost rate (per unit)"
        |>;

        newModel["Rates"] = Join[ newModel["Rates"], aNewStocks ];

        (* New and changed Equations *)

        newBySeverelyInfectedTerm = contactRate[ISSP] / TP[t] * SP[t] * Max[ISSP[t] - HP[t], 0] + contactRate[HP] / TP[t] * SP[t] * HP[t];
        newByNormallyInfectedTerm = contactRate[INSP] / TP[t] * SP[t] * INSP[t];
        newlyInfectedTerm = newBySeverelyInfectedTerm + newByNormallyInfectedTerm;

        totalNumberOfBedsTerm = nhbr[ISSP, INSP] * nahb;

        lsNewEquations = {
          If[ birthsTermQ,
            SP'[t] == deathRate[TP] * TP[t] - newlyInfectedTerm - deathRate[TP] * SP[t],
            (* ELSE *)
            SP'[t] == - newlyInfectedTerm - deathRate[TP] * SP[t]
          ],
          EP'[t] == newlyInfectedTerm - (deathRate[TP] + (1 / aincp) ) * EP[t],
          INSP'[t] == (1 - sspf[SP]) * (1 / aincp) * EP[t] - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * (1 / aincp) * EP[t] - (1 / aip) * ISSP[t] - deathRate[ISSP] * ISSP[t],
          HP'[t] == Piecewise[{{Min[HB[t] - HP[t], sspf[SP] * (1 / aincp) * EP[t]], HP[t] < HB[t]}}, 0] - (1 / aip) * HP[t] - deathRate[HP] * HP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          DIP'[t] == deathRate[ISSP] * ISSP[t] + deathRate[INSP] * INSP[t] + deathRate[HP] * HP[t],
          HB'[t] == nhbcr[ISSP, INSP],
          MSD'[t] == hpmscr[ISSP, INSP] * HP[t] + upmscr[ISSP, INSP] * (INSP[t] + ISSP[t] - HP[t]),
          MHS'[t] == hscr[ISSP, INSP] * HP[t],
          MMSP'[t] == mspcr[ISSP, INSP] * MSD[t],
          MLP'[t] == lpcr[ISSP, INSP] * (ISSP[t] + INSP[t] + DIP[t])
        };


        pos = Position[ model["Equations"], #]& /@ lsNewEquations[[All, 1]];
        pos = Flatten @ Map[ If[ Length[#] == 0, {}, First @ Flatten @ # ]&, pos ];
        newModel["Equations"] = Join[ Delete[newModel["Equations"], List /@ pos], lsNewEquations ];

        (* New Initial conditions *)
        If[ KeyExistsQ[model, "InitialConditions"],
          newModel["InitialConditions"] =
              Join[
                newModel["InitialConditions"],
                {
                  HP[0] == 0,
                  DIP[0] == 0,
                  HB[0] == nhbr[TP] * (TP[0] /. newModel["RateRules"]),
                  MSD[0] == 0,
                  MHS[0] == 0,
                  MMSP[0] == 0}
              ];
        ];

        (* New Rate Rules *)
        If[ KeyExistsQ[model, "RateRules"],
          newModel["RateRules"] =
              Join[
                newModel["RateRules"],
                <|
                  deathRate[HP] -> 0.25 * deathRate[ISSP],
                  contactRate[HP] -> 0.1 * contactRate[ISSP],
                  nhbr[TP] -> 2.9 / 1000,
                  nhbcr[ISSP, INSP] -> 0,
                  hscr[ISSP, INSP] -> 600,
                  hpmscr[ISSP, INSP] -> 4,
                  upmscr[ISSP, INSP] -> 2,
                  mspcr[ISSP, INSP] -> 120|>
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
(* ModelGridTableForm                                      *)
(***********************************************************)

Clear[ModelGridTableForm];

ModelGridTableForm[model_Association] :=
    Block[{aTHeadings},

      aTHeadings = <|
        "Stocks" -> {"Symbol", "Description"},
        "Rates" -> {"Symbol", "Description"},
        "Equations" -> {"Equation"},
        "RateRules" -> {"Symbol", "Value"},
        "InitialConditions" -> {"Equation"}
      |>;

      Association @
          KeyValueMap[
            #1 -> ResourceFunction["GridTableForm"][ If[AssociationQ[#2], List @@@ Normal[#2], List /@ #2], TableHeadings -> aTHeadings[#1] ]&,
            model
          ]
    ];


End[]; (* `Private` *)

EndPackage[]

