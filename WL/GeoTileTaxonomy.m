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
    ʇǝu ˙ oǝʇsod @ ǝqnɔuouoʇuɐ
    Windermere, Florida, USA.
*)

(* :Title: GeoTileTaxonomy *)
(* :Context: GeoTileTaxonomy` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-01-20 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 Anton Antonov *)
(* :Keywords: Tile, TileBins, Hextile, HextileBins, Taxonomy, Geo-location *)
(* :Discussion:

Here is an usage example:

dsUSAZIPCodes = ResourceFunction["ImportCSVToDataset"]["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Data/dfUSZipCodesFrom2013GovernmentData.csv"]
aZIPToLatLon = Normal@dsUSAZIPCodes[Association, #ZIP -> {#LAT, #LON} &];

AbsoluteTiming[
 aRes1 = GeoTileTaxonomy[dsUSAZIPCodes, cellSize, "TileBins", "TaxonomyName" -> "SquareTile1deg", DistanceFunction -> ChessboardDistance];
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

If[Length[DownValues[MonadicGeometricNearestNeighbors`GNNMonUnit]] == 0,
  Echo["MonadicGeometricNearestNeighbors.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicGeometricNearestNeighbors.m"];
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["GeoTileTaxonomy`"];
(* Exported symbols added here with SymbolName::usage *)

GeoTileTaxonomy::usage = "GeoTileTaxonomy[itemToCoords : (_Association | _Dataset), cellSize_?NumericQ, tileBinsType : (\"TileBins\" | \"HextileBins\"), OptionsPattern[]]
makes geo-taxonomy based on tiles.
If the argument itemToCoords is a dataset then it is expected to have the columns {\"Item\", \"Lat\", \"Lon\"}.";

GeoTileTaxonomyAdjacencyMatrix::usage = "GeoTileTaxonomyAdjacencyMatrix[aTaxonomy_Association, nnsSpec_ : Automatic] \
makes a (graph) adjacency matrix for given taxonomy.";

Begin["`Private`"];

Needs["MathematicaForPredictionUtilities`"];
Needs["MonadicGeometricNearestNeighbors`"];

(*------------------------------------------------------------*)
(* Geo tile taxonomy                                          *)
(*------------------------------------------------------------*)

Clear[GeoTileTaxonomy];

Options[GeoTileTaxonomy] = {DistanceFunction -> ChessboardDistance, "TaxonomyName" -> Automatic};

GeoTileTaxonomy[dsArg_Dataset, cellSize_?NumericQ, tileBinsType : ("TileBins" | "HextileBins"), opts : OptionsPattern[]] :=
    Block[{ds = dsArg, row, aZIPToLatLon},

      row = Normal[ds[[1]]];
      If[ AssociationQ[row] && Apply[And, KeyExistsQ[Normal[ds[[1]]], #] & /@ {"ZIP", "LAT", "LON"}],
        ds = Map[ Join[ KeyDrop[ #, {"ZIP", "LAT", "LON"} ], <| "Item" -> #["ZIP"], "Lat" -> #["LAT"], "Lon" -> #["LON"]|> ]&, ds];
      ];
      aZIPToLatLon = Normal @ ds[Association, #Item -> {#Lat, #Lon}&];

      GeoTileTaxonomy[aZIPToLatLon, cellSize, tileBinsType, opts]
    ];

GeoTileTaxonomy[aZIPToLatLon_Association, cellSize_?NumericQ, tileBinsType : ("TileBins" | "HextileBins"), opts : OptionsPattern[]] :=
    Block[{dfunc, taxonomyName, data, lsTags,
      aSquareTiles, aSquareTilesTaxonomy, dsSquareTilesTaxonomy, nf,
      dsItemCodes, dsUSAZIPCodesWithGeoTags},

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

      dsItemCodes = Dataset @ KeyValueMap[ <| "Item" -> #1, "Lat" -> #2[[1]], "Lon" -> #2[[2]]|>&, aZIPToLatLon];

      dsUSAZIPCodesWithGeoTags =
          dsItemCodes[All, Join[#, <|"Taxonomy" -> taxonomyName, "Tag" -> nf[{#Lon, #Lat}][[1]]|>] &];

      <|"Taxonomy" -> dsSquareTilesTaxonomy, "Items" -> dsUSAZIPCodesWithGeoTags, "Tiles" -> aSquareTiles, "Type" -> tileBinsType, "CellSize" -> cellSize|>
    ];

GeoTileTaxonomy[___] := $Failed;

(*------------------------------------------------------------*)
(* Nearest neighbors matrix                                   *)
(*------------------------------------------------------------*)

Clear[GeoTileTaxonomyAdjacencyMatrix];
GeoTileTaxonomyAdjacencyMatrix[ aTaxonomy_?AssociationQ, nnsSpecArg_ : Automatic, opts : OptionsPattern[] ] :=
    Block[{nnsSpec = nnsSpecArg, aCenters, gnnObj, matAdj},

      If[ TrueQ[nnsSpec === Automatic],

        If[ ! KeyExistsQ[aTaxonomy, "Type"],
          Echo["Cannot find taxonomy type in the first argument.", "GeoTileTaxonomyAdjacencyMatrix:"];
          Return[$Failed]
        ];

        If[ ! KeyExistsQ[aTaxonomy, "CellSize"],
          Echo["Cannot find cell size in the first argument.", "GeoTileTaxonomyAdjacencyMatrix:"];
          Return[$Failed]
        ];

        Which[

          aTaxonomy["Type"] == "TileBins",
          nnsSpec = {9, 1.1 * Sqrt[2] * aTaxonomy["CellSize"]},

          aTaxonomy["Type"] == "HextileBins",
          nnsSpec = {7, 1.1 / Cos[Pi / 6.] * aTaxonomy["CellSize"]},

          True,
          nnsSpec = {9, Infinity};
          Echo[
            Row[{
              "Do not know how to process taxonomy type:", Spacer[3], aTaxonomy["Type"],
              Spacer[3],
              "Continuing with nearest neighbors spec to be:", Spacer[3], ToString[nnsSpec]
            }],
            "GeoTileTaxonomyAdjacencyMatrix:"];
        ]
      ];

      If[ NumericQ[nnsSpec],
        nnsSpec = {nnsSpec, Infinity}
      ];

      aCenters = Normal[aTaxonomy["Taxonomy"][Association, #Tag -> {#CenterLon, #CenterLat} &]];

      gnnObj =
          Fold[
            GNNMonBind,
            GNNMonUnit[aCenters],
            {
              GNNMonMakeNearestFunction[DistanceFunction -> EuclideanDistance],
              GNNMonComputeThresholds[nnsSpec[[1]], "AggregationFunction" -> Mean]
            }];

      matAdj =
          Fold[GNNMonBind,
            gnnObj,
            {
              GNNMonComputeAdjacencyMatrix[nnsSpec],
              GNNMonTakeValue
            }];

      matAdj
    ];

GeoTileTaxonomyAdjacencyMatrix[___] := $Failed;

End[]; (* `Private` *)

EndPackage[]