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

BeginPackage["EpidemiologyModels`"];
(* Exported symbols added here with SymbolName::usage *)

SIRModel::usage = "SIRModel[var, con] generates SIR model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SI2RModel::usage = "SI2RModel[var, con] generates SI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

SEI2RModel::usage = "SEI2RModel[var, con] generates SEI2R model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

Begin["`Private`"];


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
        deathRate = ToExpression[ context <> "\[Delta]"],
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
              MLP[t] -> "Money of lost productivity"|>;

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
          lsEquations = Append[lsEquations, TP[t] == SP[t] + IP[t] + RP[t] ]
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
              SP[0] == (TP[t] /. aRateRules) - 2,
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

Options[SI2RModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> False, "RateRules" -> False };

SI2RModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, tpRepr,
      newlyInfectedTerm, aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SI2RModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SI2RModel, "RateRules" ] ];

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
        deathRate = ToExpression[ context <> "\[Delta]"],
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
              MLP[t] -> "Money of lost productivity"|>;

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
          SP'[t] == -newlyInfectedTerm - deathRate[TP] * SP[t],
          INSP'[t] == (1 - sspf[SP]) * newlyInfectedTerm - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * newlyInfectedTerm - (1 / aip) * ISSP[t] - deathRate[ISSP] * ISSP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          MLP'[t] == lpcr[ISSP, INSP] * (TP[t] - RP[t] - SP[t])
        };

        Which[
          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TP[t] -> ( SP[t] + INSP[t] + ISSP[t] + RP[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TP[t] == SP[t] + INSP[t] + ISSP[t] + RP[t] ]
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

Options[SEI2RModel] = { "TotalPopulationRepresentation" -> None, "InitialConditions" -> False, "RateRules" -> False  };

SEI2RModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addRateRulesQ, addInitialConditionsQ, tpRepr,
      newlyInfectedTerm, aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEI2RModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEI2RModel, "RateRules" ] ];

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
        deathRate = ToExpression[ context <> "\[Delta]"],
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
              MLP[t] -> "Money of lost productivity"|>;

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
          SP'[t] == -newlyInfectedTerm - deathRate[TP] * SP[t],
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
          lsEquations = Append[lsEquations, TP[t] == SP[t] + EP[t] + INSP[t] + ISSP[t] + RP[t] ]
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


End[]; (* `Private` *)

EndPackage[]

