(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: GeoLocationUtilities *)
(* :Context: GeoLocationUtilities` *)
(* :Author: Anton Antonov *)
(* :Date: 2025-03-11 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2025 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:


## ToAddressElements

```mathematica
ToAddressElements["730 NJ-68, Bordentown, NJ 08505, USA"]

# <|"City" -> "Bordentown", "State" -> "NJ", "ZIP" -> "08505", "Country" -> "USA"|>

ToAddressElements["730 NJ-68, Los Angeles, CA 08505, USA"]

# <|"City" -> "Los Angeles", "State" -> "CA", "ZIP" -> "08505", "Country" -> "USA"|>

ToAddressElements["Los Angeles, CA, USA"]

# <|"City" -> $Failed, "State" -> $Failed, "ZIP" -> $Failed, "Country" -> $Failed|>
```

## FromStateAbbreviation

## ToEntityObject

*)


BeginPackage["GeoLocationUtilities`"];
(* Exported symbols added here with SymbolName::usage *)

ToAddressElements::usage = "Get address elements of a string.";

FromStateAbbreviation::usage = "Convert a string to a USA state name.";

ToStateAbbreviation::usage = "Convert a string to a USA state abbreviation.";

ToEntityObject::usage = "Convert an address association into an entity.";

Begin["`Private`"];

(********************************************************************)
(* ToAddressElements                                                *)
(********************************************************************)

Clear[ToAddressElements];
ToAddressElements[s_String] :=
    Block[{res, res2},
      res = Select[Map[StringTrim, StringSplit[s, {","}]], StringLength[#] > 0 &];
      res = If[Length[res] > 3,
        res2 = res[[-3 ;; -1]];
        Flatten@ReplacePart[res2, -2 -> StringSplit[res[[-2]]]]
        ,
        (*ELSE*)
        Table[$Failed, 4]
      ];
      If[Length[res] == 4,
        AssociationThread[{"City", "State", "ZIP", "Country"}, res],
        $Failed
      ]
    ];

(********************************************************************)
(* FromStateAbbreviation                                            *)
(********************************************************************)

aStateAbbreviations = <|
  "AL" -> "Alabama", "AK" -> "Alaska", "AZ" -> "Arizona", "AR" -> "Arkansas",
  "CA" -> "California", "CO" -> "Colorado", "CT" -> "Connecticut", "DE" -> "Delaware",
  "FL" -> "Florida", "GA" -> "Georgia", "HI" -> "Hawaii", "ID" -> "Idaho",
  "IL" -> "Illinois", "IN" -> "Indiana", "IA" -> "Iowa", "KS" -> "Kansas",
  "KY" -> "Kentucky", "LA" -> "Louisiana", "ME" -> "Maine", "MD" -> "Maryland",
  "MA" -> "Massachusetts", "MI" -> "Michigan", "MN" -> "Minnesota", "MS" -> "Mississippi",
  "MO" -> "Missouri", "MT" -> "Montana", "NE" -> "Nebraska", "NV" -> "Nevada",
  "NH" -> "New Hampshire", "NJ" -> "New Jersey", "NM" -> "New Mexico", "NY" -> "New York",
  "NC" -> "North Carolina", "ND" -> "North Dakota", "OH" -> "Ohio", "OK" -> "Oklahoma",
  "OR" -> "Oregon", "PA" -> "Pennsylvania", "RI" -> "Rhode Island", "SC" -> "South Carolina",
  "SD" -> "South Dakota", "TN" -> "Tennessee", "TX" -> "Texas", "UT" -> "Utah",
  "VT" -> "Vermont", "VA" -> "Virginia", "WA" -> "Washington", "WV" -> "West Virginia",
  "WI" -> "Wisconsin", "WY" -> "Wyoming"
|>;

aStateNameToAbbreviation = Association[Reverse /@ Normal[aStateAbbreviations]];

Clear[FromStateAbbreviation];

FromStateAbbreviation[Association] := aStateAbbreviations;

FromStateAbbreviation[st_String] :=
    Lookup[aStateAbbreviations, st, If[KeyExistsQ[aStateNameToAbbreviation, Capitalize[st]], Capitalize[st], $Failed]];

Clear[ToStateAbbreviation];

ToStateAbbreviation[Association] := aStateNameToAbbreviation;

ToStateAbbreviation[st_String] :=
    Lookup[aStateNameToAbbreviation, st, If[KeyExistsQ[aStateAbbreviations, ToUpperCase[st]], ToUpperCase[st], $Failed]];

(********************************************************************)
(* ToEntityObject                                                   *)
(********************************************************************)

Clear[ToEntityObject];
ToEntityObject[a_Association] :=
    Entity["City", {a["City"], a["State"], a["Country"]} /. {"USA" -> "UnitedStates"} /. aStateAbbreviations];

End[]; (* `Private` *)

EndPackage[]