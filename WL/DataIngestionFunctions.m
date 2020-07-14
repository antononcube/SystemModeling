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

ConvertToExportSheet::usage = "Convert a dataset into a matrix that can be exported as XLS sheet.";

ExportToXLSFile::usage = "Export a complicated data structure into an XLS file. \
Associations of datasets are converted to an XLS with multiple tabs.";

Begin["`Private`"];


(************************************************************)
(* XLSSheetToDataset                                        *)
(************************************************************)

Clear[XLSSheetToDataset];

SyntaxInformation[XLSSheetToDataset] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

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


(**************************************************************)
(* ConvertToExportSheet                                       *)
(**************************************************************)

Clear[ConvertToExportSheet];

SyntaxInformation[ConvertToExportSheet] = { "ArgumentsPattern" -> { _ } };

ConvertToExportSheet[ data : ( _?VectorQ | _?MatrixQ ) ] := data;

ConvertToExportSheet[ data : Association[ (_?AtomQ -> _?AtomQ) .. ] ] :=
    Prepend[ List @@@ Normal[data], {"Key", "Value"} ] ;

ConvertToExportSheet[ dataArg_Dataset ] :=
    Block[{data = dataArg, namedRowsQ, firstRecord, colNames},

      If[ AssociationQ[Normal[data]],
        namedRowsQ = True;
        data = data[Values];
      ];

      firstRecord = Normal[data[1, All]];
      colNames = If[ AssociationQ[firstRecord], Keys[firstRecord], None ];

      Which[
        TrueQ[colNames === None],
        data = Normal @ data,

        Length[colNames] > 0,

        data = Normal[data[Values]];
        data = Prepend[data, colNames],

        True,
        Return[$Failed]
      ];

      data
    ];


(************************************************************)
(* ExportToXLSFile                                          *)
(************************************************************)

Clear[SheetDataQ];
SheetDataQ[x_] := MatchQ[x, _?VectorQ | _?MatrixQ | _Dataset | _Association ];
SheetDataQ[___] := False;

Clear[ExportToXLSFile];

SyntaxInformation[ExportToXLSFile] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

ExportToXLSFile::"nargs" = "The first argument is expected to be a file name; \
the second argument is expected to be a dataset, a list of datasets, or an association with values that are datasets;
the rest of the arguments are passed to Export.";

ExportToXLSFile[ file_String, ds_Dataset, opts:OptionsPattern[] ] :=
    Block[{},
      Export[ file, ConvertToExportSheet[ds], "Data", opts ]
    ];

ExportToXLSFile[ file_String, lsData : List[ _?SheetDataQ .. ], opts:OptionsPattern[] ] :=
    Export[file, ConvertToExportSheet /@ lsData, "Data", opts];

ExportToXLSFile[ file_String, aDatasets : (Association | List)[ (_ -> _?SheetDataQ) .. ], opts:OptionsPattern[] ] :=
    Block[{aRes},

      aRes = ConvertToExportSheet /@ aDatasets;

      Export[file, Normal[aRes], "Data", opts]
    ];

ExportToXLSFile[___] :=
    Block[{},
      Message[ExportToXLSFile::"nargs"];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]