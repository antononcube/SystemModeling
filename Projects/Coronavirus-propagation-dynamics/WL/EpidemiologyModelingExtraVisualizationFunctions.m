(*
    Epidemiology modeling extra visualization functions Mathematica package
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

(* :Title: EpidemiologyModelingExtraVisualizationFunctions *)
(* :Context: EpidemiologyModelingExtraVisualizationFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-08-06 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

The package "EpidemiologyModelingVisualizationFunctions.m" has functions that independent of the monad ECMMon.

The functions in this package most likely utilize ECMMon.

*)


(**************************************************************)
(* Load packages                                              *)
(**************************************************************)

If[Length[DownValues[MonadicEpidemiologyCompartmentalModeling`ECMMonUnit]] == 0,
  Echo["MonadicEpidemiologyCompartmentalModeling.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/MonadicEpidemiologyCompartmentalModeling.m"];
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["EpidemiologyModelingExtraVisualizationFunctions`"];

StockAndDataDateListPlot::usage = "StockAndDataDateListPlot[model_?EpidemiologyFullModelQ, stock_Symbol, tsCalibrationData_TemporalData, aCalibrationParams_Association] \
visualizes the stock simulated time series together with calibration time series.`";

Begin["`Private`"];

Needs["StateMonadCodeGenerator`"];
Needs["CrossTabulate`"];
Needs["SSparseMatrix`"];
Needs["HextileBins`"];
Needs["TileBins`"];
Needs["EpidemiologyModels`"];
Needs["EpidemiologyModelModifications`"];
Needs["EpidemiologyModelingSimulationFunctions`"];
Needs["EpidemiologyModelingVisualizationFunctions`"];
Needs["SystemDynamicsInteractiveInterfacesFunctions`"];


(**************************************************************)
(* StockAndDataDateListPlot                                   *)
(**************************************************************)

opts1 = {
  PlotRange -> All,
  AxesLabel -> {"Day", "Number of deaths"},
  DateTicksFormat -> {"Year", "-", "Month", "-", "Day"},
  AspectRatio -> 1 / 3,
  Joined -> False
};

opts2 = {
  PlotStyle -> {PointSize[0], Opacity[0.45], Orange},
  Joined -> False,
  Filling -> 0,
  FillingStyle -> Directive[CapForm[None], AbsoluteThickness[3], Orange]
};

SyntaxInformation[StockAndDataDateListPlot] = { "ArgumentsPattern" -> { _, _, _, _, OptionsPattern[] } };

Clear[StockAndDataDateListPlot];

StockAndDataDateListPlot::"optnonneg" = "The value of the option `1` is expected to be a non-negative number.";

Options[StockAndDataDateListPlot] =
    Join[
      {"CalibrationDataOffset" -> 0, "MaxTime" -> 365, "Differences" -> False},
      Options[DateListPlot]
    ];

StockAndDataDateListPlot[
  model_?EpidemiologyFullModelQ,
  stock_Symbol,
  tsCalibrationData : (_TimeSeries | _TemporalData),
  aCalibrationParameters_Association,
  opts : OptionsPattern[] ] :=

    Block[{calibrationDataOffset, maxTime, differencesQ, diffFunc = Identity,
      lsOffset, lsCalibrationData, tsCalibrationDataPadded, ecmObj, model2,
      plotLabel},

      (* Get options *)

      calibrationDataOffset = OptionValue[StockAndDataDateListPlot, "CalibrationDataOffset"];
      If[! (NumberQ[calibrationDataOffset] && calibrationDataOffset >= 0),
        Message[StockAndDataDateListPlot::"optnonneg", "\"CalibrationDataOffset\""];
        Return[$Failed]
      ];

      maxTime = OptionValue[StockAndDataDateListPlot, "MaxTime"];
      If[! (NumberQ[maxTime] && maxTime >= 0),
        Message[StockAndDataDateListPlot::"optnonneg", "\"MaxTime\""];
        Return[$Failed]
      ];

      differencesQ = OptionValue[StockAndDataDateListPlot, "Differences"];
      If[differencesQ, diffFunc = Differences];

      (* Pad data *)
      lsOffset = ConstantArray[0, calibrationDataOffset];
      lsCalibrationData = Join[lsOffset, tsCalibrationData["Values"]];
      tsCalibrationDataPadded =
          TimeSeries@
              Transpose[{DatePlus[Min[tsCalibrationData["Times"]], # - Length[lsOffset]] & /@ Range[0, Length[#] - 1], #}] &[lsCalibrationData];

      (* Make model object *)
      model2 = model;

      (* Simulate *)
      ecmObj =
          Fold[
            ECMMonBind,
            ECMMonUnit[],
            {
              ECMMonSetSingleSiteModel[model2],
              ECMMonAssignRateRules[aCalibrationParameters],
              ECMMonSimulate[maxTime]
            }];

      (* Find plot label *)

      plotLabel = Cases[{opts}, HoldPattern[PlotLabel -> _]];
      If[Length[plotLabel] == 0, plotLabel = stock, plotLabel = plotLabel[[1, 2]]];

      (* Visualize *)
      Legended[
        Show[{
          DateListPlot[tsCalibrationDataPadded, FilterRules[{opts}, DateListPlot],
            PlotRange -> {0, Automatic}, opts1, opts2],
          PrefixGroupsSolutionsListPlot[
            model2,
            Map[diffFunc, Fold[ ECMMonBind, ecmObj, {ECMMonGetSolutionValues[stock, maxTime], ECMMonTakeValue}]],
            "DateListPlot" -> True,
            "StartDate" -> tsCalibrationDataPadded["Times"][[1]],
            PlotLegends -> None]
        }, PlotLabel -> plotLabel],
        SwatchLegend[{Orange, Lighter@Blue}, {"Calibration data", "Model results"}]]
    ];


End[]; (* `Private` *)

EndPackage[]