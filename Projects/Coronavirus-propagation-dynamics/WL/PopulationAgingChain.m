(*
    Population Aging Chain Mathematica package
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

(* :Title: PopulationAgingChain *)
(* :Context: PopulationAgingChain` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-10-17 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["PopulationAgingChain`"];
(* Exported symbols added here with SymbolName::usage *)

PopulationAgingChain::usage = "Makes population aging chain system of equations.";

Begin["`Private`"];

Clear[PopulationAgingChain];

SyntaxInformation[PopulationAgingChain] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

PopulationAgingChain::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

PopulationAgingChain::"abv" = "The value of the option \"AgeBreaks\" is expected to be a vector of positive numbers.";

PopulationAgingChain::"abl" = "The value of the option \"AgeBreaks\" is expected to be a vector of least two positive numbers.";

PopulationAgingChain::"sopt" = "The value of the option \"`1`\" is expected to be a string.";

Options[PopulationAgingChain] := {
  "AgeBreaks" -> Automatic,
  "AgeGroupNames" -> False,
  "PopulationSymbolName" -> "P",
  "ImmigrationSymbolName" -> "R",
  "MaturationSymbolName" -> "M",
  "BirthRateSymbolName" -> "\[Lambda]",
  "DeathRateSymbolName" -> "\[Mu]",
  "ExitRateSymbolName" -> "\[Xi]",
  "TotalFertilitySymbolName" -> "TF",
  "ChildbearingYearSymbolName" -> "CY",
  "ChildbearingWeightsSymbolName" -> "w",
  "YearsPerCohortSymbolName" -> "YPC",
  "SurvivalFractionSymbolName" -> "SF",
  "FractionalDeathRateSymbolName" -> "FDR",
  "SexRatioSymbolName" -> "\[Sigma]",
  "FirstChildbearingYear" -> 13,
  "LastChildbearingYear" -> 50
};

PopulationAgingChain[ t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{lsAgeBreaks, firstFerYear, lastFerYear,
      lsAgeGroups, ageGroupNamesQ, aAgeGroups, aSymbolNames,
      aStocks, aRates, aFertileRanges, aFertileFractions, lsEquations, n, ind, indp,
      aRes},

      (* Age breaks *)
      lsAgeBreaks = OptionValue[ PopulationAgingChain, "AgeBreaks" ];
      If[ TrueQ[ lsAgeBreaks === Automatic ], lsAgeBreaks = Range[0, 80, 10] ];
      If[ ! ( VectorQ[ lsAgeBreaks, NumberQ] && Apply[ And, # >= 0 & /@ lsAgeBreaks ]),
        Message[PopulationAgingChain::abv];
        Return[$Failed]
      ];

      If[ Length[lsAgeBreaks] < 2,
        Message[PopulationAgingChain::abl];
        Return[$Failed]
      ];

      lsAgeBreaks = Union[ lsAgeBreaks, {0, Infinity}];

      (* Age group names *)
      ageGroupNamesQ = TrueQ[ OptionValue[ PopulationAgingChain, "AgeGroupNames" ] ];

      (* All symbol name options *)
      aSymbolNames = Association @ Map[ # -> OptionValue[ PopulationAgingChain, #]&, Select[ Keys[Options[PopulationAgingChain]], StringMatchQ[#, __ ~~ "SymbolName" ]& ] ];

      Do[
        If[ !StringQ[ aSymbolNames[k] ],
          Message[PopulationAgingChain::sopt, k];
          Return[$Failed]
        ],
        {k, Keys[aSymbolNames]}
      ];

      (* First Childbearing Year *)
      firstFerYear = OptionValue[ PopulationAgingChain, "FirstChildbearingYear" ];

      (* Last Childbearing Year *)
      lastFerYear = OptionValue[ PopulationAgingChain, "LastChildbearingYear" ];

      (* Age groups *)
      lsAgeGroups = Partition[ lsAgeBreaks, 2, 1];
      lsAgeGroups = Prepend[Map[# + {1, 0} &, Rest @ lsAgeGroups], First @ lsAgeGroups];

      If[ ageGroupNamesQ,
        aAgeGroups = AssociationThread[ Map[ ToString[#[[1]]] <> "-" <> ToString[#[[2]]]&, lsAgeGroups], lsAgeGroups],
        aAgeGroups = AssociationThread[ Range[0, Length[lsAgeGroups] - 1], lsAgeGroups]
      ];

      With[{
        P = ToExpression[ context <> aSymbolNames["PopulationSymbolName"]],
        I = ToExpression[ context <> aSymbolNames["ImmigrationSymbolName"]],
        M = ToExpression[ context <> aSymbolNames["MaturationSymbolName"]],
        birthRate = ToExpression[ context <> aSymbolNames["BirthRateSymbolName"]],
        deathRate = ToExpression[ context <> aSymbolNames["DeathRateSymbolName"]],
        exitRate = ToExpression[ context <> aSymbolNames["ExitRateSymbolName"]],
        TF = ToExpression[ context <> aSymbolNames["TotalFertilitySymbolName"]],
        CY = ToExpression[ context <> aSymbolNames["ChildbearingYearSymbolName"]],
        w = ToExpression[ context <> aSymbolNames["ChildbearingWeightsSymbolName"]],
        sr = ToExpression[ context <> aSymbolNames["SexRatioSymbolName"]],
        YPC = ToExpression[ context <> aSymbolNames["YearsPerCohortSymbolName"]],
        SF = ToExpression[ context <> aSymbolNames["SurvivalFractionSymbolName"]],
        FDR = ToExpression[ context <> aSymbolNames["FractionalDeathRateSymbolName"]]
      },

        (* Stocks *)
        aStocks =
            <|TF[t] -> "Total Fertility"|>;

        (* Rates  *)
        aRates = <||>;

        (* Equations  *)
        n = Length[aAgeGroups] - 1;
        If[ TrueQ[ firstFerYear === Automatic ], firstFerYear = 13 ];
        If[ TrueQ[ lastFerYear === Automatic ], lastFerYear = 50 ];

        (* Here for each age group we find the proportion of it that has fertile females *)
        aFertileRanges =
            First /@ Select[Map[IntervalIntersection[Interval[{13, 50}], Interval[#]] &, aAgeGroups], Length[#] > 0 &];

        aFertileFractions =
            Association@
                KeyValueMap[#1 -> (#2[[2]] - #2[[1]] + 1) / (
                  aAgeGroups[#1][[2]] - aAgeGroups[#1][[1]] + 1) &, aFertileRanges];

        lsEquations =
            Join @@
                Table[
                  Join[
                    MapThread[
                      Function[{agName, ag, i},
                        ind = Keys[aAgeGroups][[i + 1]];
                        indp = If[ i > 0, Keys[aAgeGroups][[i]]];

                        aStocks =
                            Join[
                              aStocks,
                              <| P[S, ind][t] -> "Population of sex " <> S <> " in age group " <> ToString[agName] |>
                            ];

                        aRates =
                            Join[
                              aRates,
                              <|birthRate[S] -> "Birth rate for sex " <> S,
                                deathRate[S, ind] -> "Death rate for age group " <> ToString[agName],
                                M[S, ind][t] -> "Maturation rate for age group " <> ToString[agName],
                                I[S, ind] -> "Immigration rate for age group " <> ToString[agName],
                                exitRate[S, ind][t] -> "Exit rate for age group " <> ToString[agName],
                                FDR[S, ind] -> "Fractional death rate for age group " <> ToString[agName],
                                YPC[ind] -> "Years in age group " <> ToString[agName]|>
                            ];

                        Which[
                          i == 0,
                          P[S, ind]'[t] == birthRate[S] + I[S, ind] - deathRate[S, ind] - M[S, ind],

                          i < n,
                          P[S, ind]'[t] == M[S, indp] + I[S, ind] - deathRate[S, ind] - M[S, ind],

                          i == n,
                          P[S, ind]'[t] == M[S, indp] + I[S, ind] - deathRate[S, ind]
                        ]
                      ],
                      {Keys[aAgeGroups], Values[aAgeGroups], Range[0, Length[aAgeGroups] - 1]}
                    ],
                    {
                      birthRate[S] ==
                          sr[S] * ( TF / (lastFerYear - firstFerYear + 1 ) ) *
                              Total[ Table[ w[a] * aFertileFractions[a] * P["female", a], {a, Keys[aFertileFractions]}]]
                    },
                    Table[ M[S, ind][t] == exitRate[S, ind][t] * SF[S, ind], {ind, Keys[aAgeGroups]} ],
                    Table[ SF[S, ind] == Exp[ - FDR[S, ind] ] * YPC[ind], {ind, Keys[aAgeGroups]} ],
                    Table[ exitRate[S, ind][t] == P[S, ind][t] / YPC[ind], {ind, Keys[aAgeGroups]} ]
                  ],
                  {S, {"male", "female"}}
                ];

        lsEquations =
            Join[
              lsEquations,
              {Total[Table[ w[a], {a, Keys[aFertileFractions]}]] == 1},
              Table[ YPC[ind] == aAgeGroups[ind][[2]] - aAgeGroups[ind][[1]] + 1, {ind, Keys[aAgeGroups]}]
            ];

        (* Result *)
        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations, "AgeGroups" -> aAgeGroups |>;
        aRes
      ]
    ];

PopulationAgingChain[___] :=
    Block[{},
      Message[PopulationAgingChain::"nargs"];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]