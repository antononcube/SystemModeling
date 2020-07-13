(*
    Data Ingestion Functions Mathematica package
    Copyright (C) 2020  Anton Antonov

    BSD 3-Clause License

    Copyright (c) 2020, Anton Antonov
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this
      list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
    OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

(* :Title: DataIngestionFunctions *)
(* :Context: DataIngestionFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-07-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["DataIngestionFunctions`"];

XLSSheetToDataset::usage = "Convert a list of lists into a Dataset object";

Begin["`Private`"];

Clear[XLSSheetToDataset];

Options[XLSSheetToDataset] = {
  "SheetOffset" -> 12, "TableRowsOffset" -> 9, "TableColumnsOffset" -> 1,
  "StringColumnNames" -> False, "ExtendColumnNames" -> False
};

XLSSheetToDataset[sheet : {_List ..}, opts : OptionsPattern[]] :=
    Block[{lsColumnNames, lsDataRows, sheetOffset, tableRowsOffset, tableColumnsOffset, stringColumnNamesQ, extendColumnNamesQ},

      sheetOffset = OptionValue[XLSSheetToDataset, "SheetOffset"];
      tableRowsOffset = OptionValue[XLSSheetToDataset, "TableRowsOffset"];
      tableColumnsOffset = OptionValue[XLSSheetToDataset, "TableColumnsOffset"];
      stringColumnNamesQ = TrueQ[OptionValue[XLSSheetToDataset, "StringColumnNames"]];
      extendColumnNamesQ = TrueQ[OptionValue[XLSSheetToDataset, "ExtendColumnNames"]];

      lsColumnNames = sheet[[sheetOffset + 1, tableColumnsOffset + 1 ;; -1]];

      If[stringColumnNamesQ,
        lsColumnNames = ToString /@ lsColumnNames
      ];

      If[extendColumnNamesQ,
        lsColumnNames = Fold[ Append[#1, If[ #2 == "", Last[#1], #2]]&, {First[lsColumnNames]}, Rest[lsColumnNames] ]
      ];

      lsColumnNames =
          MapThread[If[#1 == "", #2, #1] &, {lsColumnNames, "Column-" <> ToString[#] & /@ Range[Length[lsColumnNames]]}];

      lsDataRows = sheet[[sheetOffset + 1 + tableRowsOffset + 1 ;; -1, tableColumnsOffset + 1 ;; -1]];

      (*Print[Length[lsColumnNames]];
      Print[Dimensions[Dataset[lsDataRows]]];*)

      Dataset[lsDataRows][All, AssociationThread[lsColumnNames, #] &]
    ];

End[]; (* `Private` *)

EndPackage[]