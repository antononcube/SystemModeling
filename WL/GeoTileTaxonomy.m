(*
    Geo-Tile Taxonomy Mathematica package
    Copyright (C) 2022  Anton Antonov

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

(* :Title: GeoTileTaxonomy *)
(* :Context: GeoTileTaxonomy` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-01-20 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 antonov *)
(* :Keywords: Tile, TileBins, Hextile, HextileBins, Taxonomy, Geo-location *)
(* :Discussion:

Here is an usage example:

dsUSAZIPCodes = ResourceFunction["ImportCSVToDataset"]["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Data/dfUSZipCodesFrom2013GovernmentData.csv"]
aZIPToLatLon = Normal@dsUSAZIPCodes[Association, #ZIP -> {#LAT, #LON} &];

AbsoluteTiming[
 aRes1 = MakeGeoTaxonomyDatasets[dsUSAZIPCodes, cellSize, "TileBins", "TaxonomyName" -> "SquareTile1deg", DistanceFunction -> ChessboardDistance];
]

*)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(**************************************************************)
(* Load packages                                              *)
(**************************************************************)

If[Length[DownValues[MathematicaForPredictionUtilities`ToAutomaticKeysAssociation]] == 0,
      Echo["MathematicaForPredictionUtilities.m", "Importing from GitHub:"];
      Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"];
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["GeoTileTaxonomy`"];
(* Exported symbols added here with SymbolName::usage *)

GeoTileTaxonomy::usage = "Make geo-taxonomy based on tiles.";

Begin["`Private`"];

Needs[MathematicaForPredictionUtilities];

Clear[GeoTileTaxonomy];

Options[GeoTileTaxonomy] = {DistanceFunction -> ChessboardDistance, "TaxonomyName" -> Automatic};

GeoTileTaxonomy[dsUSAZIPCodes_Dataset, cellSize_?NumericQ, tileBinsType : ("TileBins" | "HextileBins"), opts : OptionsPattern[]] :=
    Block[{dfunc, taxonomyName, data, aZIPToLatLon, lsTags,
      aSquareTiles, aSquareTilesTaxonomy, dsSquareTilesTaxonomy, nf,
      dsUSAZIPCodesWithGeoTags},

      aZIPToLatLon = Normal@dsUSAZIPCodes[Association, #ZIP -> {#LAT, #LON} &];

      dfunc = OptionValue[GeoTileTaxonomy, DistanceFunction];
      taxonomyName = OptionValue[GeoTileTaxonomy, "TaxonomyName"];
      If[taxonomyName === Automatic, taxonomyName = RandomWord[]];

      data = Reverse /@ Values[aZIPToLatLon];

      aSquareTiles = ResourceFunction[tileBinsType][data, cellSize];
      lsTags = Sort[Keys[aSquareTiles]];

      aSquareTilesTaxonomy =
          Map[<|"CenterLon" -> N[Mean[PolygonCoordinates[#]]][[1]],
            "CenterLat" -> N[Mean[PolygonCoordinates[#]]][[2]],
            "Coordinates" -> PolygonCoordinates[#]|> &, lsTags];

      aSquareTilesTaxonomy = ToAutomaticKeysAssociation[aSquareTilesTaxonomy, "tile"];

      aSquareTilesTaxonomy = KeyValueMap[Prepend[#2, "Tag" -> #1] &, aSquareTilesTaxonomy];
      dsSquareTilesTaxonomy = Dataset[aSquareTilesTaxonomy];

      dsSquareTilesTaxonomy =
          dsSquareTilesTaxonomy[All,
            Append[#, "Coordinates" -> StringTrim[StringReplace[ExportString[#Coordinates, "JSON"], WhitespaceCharacter -> ""]]] &];

      dsSquareTilesTaxonomy = dsSquareTilesTaxonomy[All, Prepend[#, "Taxonomy" -> taxonomyName] &];

      nf = Nearest[Normal@dsSquareTilesTaxonomy[All, {#CenterLon, #CenterLat} -> #Tag &], DistanceFunction -> dfunc];

      dsUSAZIPCodesWithGeoTags =
          dsUSAZIPCodes[All, Join[#, <|"Taxonomy" -> taxonomyName, "Tag" -> nf[{#LON, #LAT}][[1]]|>] &];

      <|"Taxonomy" -> dsSquareTilesTaxonomy, "ZIPCodes" -> dsUSAZIPCodesWithGeoTags|>
    ];

End[]; (* `Private` *)

EndPackage[]