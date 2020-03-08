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

Begin["`Private`"];


(***********************************************************)
(* SIR                                                     *)
(***********************************************************)

Clear[SIRStocks];
SIRStocks[t_Symbol, context_ : "Global`"] :=
    With[{
      TP = ToExpression[ context <> "TP"],
      SP = ToExpression[ context <> "SP"],
      INSP = ToExpression[ context <> "INSP"],
      ISSP = ToExpression[ context <> "ISSP"],
      RP = ToExpression[ context <> "RP"],
      MLP = ToExpression[ context <> "MLP"]
    },

      <|TP[t] -> "Total Population" ,
        SP[t] -> "Susceptible Population",
        INSP[t] -> "Infected Normally Symptomatic Population",
        ISSP[t] -> "Infected Severely Symptomatic Population",
        RP[t] -> "Recovered Population",
        MLP[t] -> "Money of lost productivity"|>
    ];

SIRRates[t_Symbol, context_ : "Global`" ] :=
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
      <|
        deathRate[TP] -> "Population death rate",
        deathRate[INSP] -> "Infected Normally Symptomatic Population death rate",
        deathRate[ISSP] -> "Infected Severely Symptomatic Population death rate",
        sspf[SP] -> "Severely Symptomatic Population Fraction" ,
        contactRate[INSP] -> "Contact rate for the normally symptomatic population",
        contactRate[ISSP] -> "Contact rate for the severely symptomatic population",
        aip -> "Average infectious period",
        lpcr[ISSP, INSP] -> "Lost productivity cost rate (per person per day)"
      |>];

Clear[SIRModel];
SIRModel[t_Symbol, context_ : "Global`" ] :=
    Block[{newlyInfectedTerm, lsEquations},
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
        newlyInfectedTerm = contactRate[ISSP] / TP[t] * SP[t] * ISSP[t] + contactRate[INSP] / TP[t] * SP[t] * INSP[t];

        lsEquations = {
          SP'[t] == -newlyInfectedTerm - deathRate[TP] * SP[t],
          INSP'[t] == (1 - sspf[SP]) * newlyInfectedTerm - (1 / aip) * INSP[t] - deathRate[INSP] * INSP[t],
          ISSP'[t] == sspf[SP] * newlyInfectedTerm - (1 / aip) * ISSP[t] - deathRate[ISSP] * ISSP[t],
          RP'[t] == (1 / aip) * (ISSP[t] + INSP[t]) - deathRate[TP] * RP[t],
          MLP'[t] == lpcr[ISSP, INSP] * (TP[t] - RP[t] - SP[t])
        };

        <| "Stocks" -> SIRStocks[t, context], "Rates" -> SIRRates[t, context], "Equations" -> lsEquations |>
      ]
    ];


End[]; (* `Private` *)

EndPackage[]

