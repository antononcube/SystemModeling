(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: UrbanFireModels *)
(* :Context: UrbanFireModels` *)
(* :Author: Anton Antonov *)
(* :Date: 2025-03-15 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2025 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["UrbanFireModels`"];
(* Exported symbols added here with SymbolName::usage *)

SEIBModel::usage = "SEIBModel[var, con] generates SEIB (urban fire) model stocks, rates, and equations \
using the time variable var with symbols in the context con.";

Begin["`Private`"];

(***********************************************************)
(* SEIB                                                    *)
(***********************************************************)

Clear[SEIBModel];

SyntaxInformation[SEIBModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEIBModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEIBModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEIBModel] = {
  "TotalPopulationRepresentation" -> None,
  "InitialConditions" -> True,
  "RateRules" -> True,
  "BirthsTerm" -> False,
  "MoneyTracking" -> True };

SEIBModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addInitialConditionsQ, addRateRulesQ, birthsTermQ, moneyTrackingQ, tpRepr,
      newlyInfectedTerm, aStocks, aRates, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEIBModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEIBModel, "RateRules" ] ];

      birthsTermQ = TrueQ[ OptionValue[SEIBModel, "BirthsTerm"] ];

      moneyTrackingQ = TrueQ[ OptionValue[ SEIBModel, "MoneyTracking" ] ];

      tpRepr = OptionValue[ SEIBModel, "TotalPopulationRepresentation" ];
      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEIBModel::"ntpval"];
        $Failed
      ];

      With[{
        TF = ToExpression[ context <> "TF"],
        SF = ToExpression[ context <> "SF"],
        EF = ToExpression[ context <> "EF"],
        IF = ToExpression[ context <> "IF"],
        BF = ToExpression[ context <> "BF"],
        MLP = ToExpression[ context <> "MLP"],
        removalRate = ToExpression[ context <> "\[Mu]"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        aip = ToExpression[ context <> "aip"],
        aincp = ToExpression[ context <> "aincp"],
        lpcr = ToExpression[ context <> "lpcr"]
      },

        (* Stocks *)
        aStocks =
            <|TF[t] -> "Total Fuel" ,
              SF[t] -> "Susceptible Fuel",
              EF[t] -> "Exposed Fuel",
              IF[t] -> "In-flames Fuel",
              BF[t] -> "Burned Fuel",
              MLP[t] -> "Money of Lost Property"|>;

        If[ !moneyTrackingQ,
          aStocks = Most @ aStocks;
        ];

        (* Rates  *)
        aRates =
            <|
              removalRate[TF] -> "Fuel removal rate",
              removalRate[IF] -> "In-flames Fuel removal rate",
              contactRate[IF] -> "Contact rate for the in-flames fuel",
              aip -> "Average in-flames period",
              aincp -> "Average incubation period",
              lpcr[IF] -> "Lost property cost rate (per fuel cell per second)"
            |>;

        If[ !moneyTrackingQ,
          aRates = Most @ aRates;
        ];

        (* Equations  *)
        (* Equations  *)
        newlyInfectedTerm = contactRate[IF] / TF[t] * SF[t] * IF[t];

        lsEquations = {
          If[ birthsTermQ,
            SF'[t] == removalRate[TF] * TF[t] - newlyInfectedTerm - removalRate[TF] * SF[t],
            (* ELSE *)
            SF'[t] == - newlyInfectedTerm - removalRate[TF] * SF[t]
          ],
          EF'[t] == newlyInfectedTerm - (removalRate[TF] + (1 / aincp) ) * EF[t],
          IF'[t] == (1 / aincp) * EF[t] - (1 / aip) * IF[t] - removalRate[IF] * IF[t],
          BF'[t] == (1 / aip) * IF[t] - removalRate[TF] * BF[t],
          MLP'[t] == lpcr[IF] * (TF[t] - BF[t] - EF[t])
        };
        If[ !moneyTrackingQ,
          lsEquations = Most @ lsEquations
        ];

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsEquations = lsEquations /. TF[t] -> TF[0],

          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. TF[t] -> ( SF[t] + EF[t] + IF[t] + BF[t] ),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, TF[t] == Max[ 0, SF[t] + EF[t] + IF[t] + BF[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <| TF[0] -> 100000,
              removalRate[TF] -> 0,
              removalRate[IF] -> 0 / aip,
              contactRate[IF] -> 0.15,
              aip -> 26,
              aincp -> 6,
              lpcr[IF] -> 0.01
            |>;

        If[ !moneyTrackingQ,
          aRateRules = Most @ aRateRules
        ];

        (* Initial conditions *)
        aInitialConditions = {
          SF[0] == (TF[0] /. aRateRules) - 1,
          EF[0] == 0,
          IF[0] == 1,
          BF[0] == 0,
          MLP[0] == 0};

        If[ !moneyTrackingQ,
          aInitialConditions = Most @ aInitialConditions
        ];

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, TF[0] == (TF[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, TF[0]]
        ];

        If[ addRateRulesQ,
          aRes = Append[aRes, "RateRules" -> aRateRules]
        ];

        If[ addInitialConditionsQ,
          aRes = Append[aRes, "InitialConditions" -> aInitialConditions];
        ];

        If[ !moneyTrackingQ,
          aRes = Most /@ aRes
        ];

        aRes
      ]
    ];

SEIBModel[___] :=
    Block[{},
      Message[SEIBModel::"nargs"];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]