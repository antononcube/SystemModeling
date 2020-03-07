(*
    System dynamics interactive interfaces functions Mathematica package
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
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["EpidemiologyModels`"];
(* Exported symbols added here with SymbolName::usage *)

SIRModel::usage = "Generates a SIR model stocks, rates, and equations.";

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
      MLP = ToExpression[ context <> "TP"]
    },

      <|"Total Population" -> TP[t],
        "Susceptible Population" -> SP[t],
        "Infected Normally Symptomatic Population" -> INSP[t],
        "Infected Severely Symptomatic Population" -> ISSP[t],
        "Recovered Population" -> RP[t],
        "Money of lost productivity" -> MLP[t]|>
    ];

SIRRates[t_Symbol, context_ : "Global`" ] :=
    With[{
      TP = ToExpression[ context <> "TP"],
      SP = ToExpression[ context <> "SP"],
      INSP = ToExpression[ context <> "INSP"],
      ISSP = ToExpression[ context <> "ISSP"],
      RP = ToExpression[ context <> "RP"],
      MLP = ToExpression[ context <> "TP"],
      deathRate = ToExpression[ context <> "\[Delta]"],
      sspf = ToExpression[ context <> "sspf"],
      contactRate = ToExpression[ context <> "\[Beta]"],
      aip = ToExpression[ context <> "aip"],
      lpcr = ToExpression[ context <> "lpcr"]
    },
      <|
        "Population death rate" -> deathRate[TP],
        "Infected Normally Symptomatic Population death rate" -> deathRate[INSP],
        "Infected Severely Symptomatic Population death rate" -> deathRate[ISSP],
        "Severely Symptomatic Population Fraction" -> sspf[SP],
        "Contact rate for the normally symptomatic population" -> contactRate[INSP],
        "Contact rate for the severely symptomatic population" -> contactRate[ISSP],
        "Average infectious period" -> aip,
        "Lost productivity cost rate (per person per day)" -> lpcr[ISSP, INSP]
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
        MLP = ToExpression[ context <> "TP"],
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

