# WirVsVirus hackathon multi-site SEI2R over a hexagonal grid graph

**Version 0.7**

Anton Antonov  
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)  
[SystemModeling at GitHub](https://github.com/antononcube/SystemModeling)  
March 2020  

## Introduction

This notebook has simulations for the [WirVsVirus Hackathon](https://wirvsvirushackathon.org) (held March 20-22, 2020). 

The notebook is based on previously developed framework for simulations with multi-site epidemiological models. [AA2, AA3, AAp1, AAp2, AAp3].

For the derivation of the graph and traveling patterns matrix used in [AA3] in this notebook we utilize an ad hoc solution using GeoHistogram’s output. (Replicating the previous approach using Voronoi mesh over USA population density data was problematic.)

The bigger picture for this notebook can seen in the project management files [AAo1, AAo2].

### Data

For the model in this notebook we use the dataset [WRI2] that associates $\approx 12500$ German cities with their populations and geographics coordinates. (The data was retrieved using Mathematica’s function [CityData](https://reference.wolfram.com/language/ref/CityData.html).)

### General assumptions (for COVID-19)

The general assumptions about COVID-19 and related mathematical modeling are listed in [AA1].

The most important features of COVID-19 used in the model here are following.

- We have two types of infected populations: severely symptomatic and normally symptomatic.

- The severely symptomatic population is 20% of the infected population.

- *Please see the full list in [AA1].*

### Specific assumptions (for this model)

- Germany’s territory is partitioned with a grid of hexagon cells. For each cell the population is obtained from [WRI2].

- People from each cell of the obtained hexagonal grid can travel to the neighboring cells. 

- All trips finish within a day.

- We consider grids that have cells with radius 50-70 km to be adequate.

- After a quarantine is enforced the usual, "everything is normal" traffic patterns become much less representative of the quarantine-time traffic patterns. 

    - *Hackathon-wise:* We did not produce actual traffic data quickly enough, and when we did, other tasks become more important in view of the approaching deadlines.

- A quarantine decreases the contact rates with a certain constant factor smaller than 1.

- Only severely symptomatic people are hospitalized.

- The hospitals have limited number of beds.

### Additional points & observations

- Right now (2020-03-22 07:40) I simulate Germany as a close system. But we can easily add, say, neighboring countries as graph nodes.  
Also, we can have “entry nodes” — international airports or train stations, border check points.

- Note that if quarantine scenarios are enacted / simulated then comprehensive traffic traveling patters data is less important — people would be staying at home.

### The single-site models

#### SEI2R (almost standard)

#### SEI2HR (includes hospitalization population and parameters)

#### SEI2HR-**Econ** (economics extensions: medical supplies production and delivery...)

### Future plans

## *ToDo list*

- [X] Implement global quarantine scenarios.

    - This includes the specification of parameters for quarantine start, duration, contact rate factor, traffic fraction factor.

- [ ] Expose/assign SEI2R/SEI2HR parameters in the section “Parameters”.

    - *Fundamental*: contact rates, incubation period, infectious period.

    - *Additional*: number of hospital beds per 1000 people, births inclusion.

- [ ] Experiment with other single-site models.

    - [X] SEI2HR

    - [ ] SEI2HR-Econ

- [ ] Implement site-dependent quarantine scenarios.

- [ ] Implement “injection” of infected people in arbitrary time and location.

    - This requires handling of relevant parameters.

    - Ideally the location is given with Geo-coordinates.

- [ ] General explanations.

    - [ ] How is this notebook used with better data?

    - [ ] The place of this notebook in the big picture strategy for Virus-Econ model. (For the hackathon.)

- [ ] Discussion of different experimental results. (Probably best to be in a separate notebook.)

## Parameters

In this section we specify the parameters for the computations.

### Graph parameters

Cell radius for the hexagonal grid:

```mathematica
cellRadius = Quantity[75, "Kilometers"];
```

Traffic factor for the traveling matrix:

```mathematica
trafficFraction = 0.12;
```

### Simulation parameters

```mathematica
singleSiteModelFunc = SEI2HRModel;
```

```mathematica
 (* max simulation time *)
maxTime = 365;
```

```mathematica
(* should births term be included in the equations? *)

includeBirthsTermQ = False;
```

#### Quarantine parameters

```mathematica
(* when the quarantine starts.? *)
quarantineStart = 65;
```

```mathematica
(* how long the quarantine is? *)
quarantineDuration = 8*7; 
```

```mathematica
(* how much the "usual" contact number decreases? *)

quarantineContactRateFactor = 0.25;
```

```mathematica
(* how much the traffic decreases? *)

quarantineTrafficFractionFactor = 0.05;
```

#### Initial conditions parameters

```mathematica
lsPatientZeroNodeInds = Automatic;
```

### Additional parameters

```mathematica
exportFileNamePrefix = "WirVsVirus-";
```

```mathematica
exportSolutionsQ = True;
```

```mathematica
renderGraphPlotsQ = True;
```

```mathematica
renderSolultionsPlotsQ = True;
```

```mathematica
renderGraphicsOfSolultionsOverGraphQ = False;
```

```mathematica
renderSolutionAnimationQ = False;
```

## Load packages

The epidemiological models framework used in this notebook is implemented with the packages [AAp1, AAp2, AA3]; the interactive plots functions are from the package [AAp4].

```mathematica
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/\
master/Projects/Coronavirus-propagation-dynamics/WL/\
EpidemiologyModels.m"]
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/\
master/Projects/Coronavirus-propagation-dynamics/WL/\
EpidemiologyModelModifications.m"]
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/\
master/Projects/Coronavirus-propagation-dynamics/WL/\
EpidemiologyModelingVisualizationFunctions.m"]
Import["https://raw.githubusercontent.com/antononcube/SystemModeling/\
master/WL/SystemDynamicsInteractiveInterfacesFunctions.m"]
```

## Ingest data

The data was previously ingested in a separate notebook. Here we ingest it directly from GitHub:

```mathematica
dsCityRecords = 
  ResourceFunction["ImportCSVToDataset"][
   "https://raw.githubusercontent.com/antononcube/SystemModeling/\
master/Data/dfGermanyCityRecords.csv"];
Dimensions[dsCityRecords]

(*{12538, 6}*)
```

```mathematica
SeedRandom[232];
RandomSample[dsCityRecords, 4]
```

![0xpfw8w1yc61r](img/0xpfw8w1yc61r.png)

## Simple data analysis

Summary of the data:

```mathematica
ResourceFunction["RecordsSummary"][dsCityRecords]
```

![0edws5iqoilre](img/0edws5iqoilre.png)

We can see that $\approx 20$% of the cities correspond to $\approx 80$% of the population.

```mathematica
ResourceFunction["ParetoPrinciplePlot"][
 Normal[dsCityRecords[All, "Population"]]]
```

![1mw9hzr4pb6bi](img/1mw9hzr4pb6bi.png)

## Make the hexagon cells graph

In this section we make a *heuristic* traveling patterns graph based on a hexagonal cells Geo-histogram of the German population.

### Geo-histogram object

```mathematica
aCoordsToPopulations = 
  AssociationThread[
   Values /@ Normal[dsCityRecords[All, {"Lat", "Lon"}]], 
   Normal[dsCityRecords[All, "Population"]]];
```

```mathematica
grHist = GeoHistogram[aCoordsToPopulations, cellRadius, 
   ColorFunction -> (Opacity[#, Blue] &), PlotLegends -> Automatic];
If[TrueQ[renderGraphPlotsQ], grHist]
```

![0yq3hnb4b7pjx](img/0yq3hnb4b7pjx.png)

### Extract histogram cells (bins)

```mathematica
Count[grHist[[1]], 
 Tooltip[h_Polygon /; MatrixQ[h[[1]]], 
  pop_ /; NumberQ[pop] && pop > 3], \[Infinity]]

(*133*)
```

```mathematica
lsCells = 
  Cases[grHist[[1]], 
   Tooltip[h_Polygon /; MatrixQ[h[[1]]], 
     pop_ /; NumberQ[pop] && pop > 3] :> <|"Cell" -> h, 
     "Population" -> pop|>, \[Infinity]];
Length[lsCells]

(*133*)
```

```mathematica
lsCells = 
  Map[Append[#, "Center" -> Mean[PolygonCoordinates[#["Cell"]]]] &, 
   lsCells];
```

```mathematica
lsCells = SortBy[lsCells, #["Center"] &];
```

```mathematica
aCells = AssociationThread[Range[Length[lsCells]], lsCells];
aCells = Association@
   KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];
```

Preliminary check:

```mathematica
If[TrueQ[renderGraphPlotsQ],
 Graphics[{FaceForm[GrayLevel[0.9]], EdgeForm[Blue], 
   Values[#["Cell"] & /@ aCells], Red, 
   Point[Values[#["Center"] & /@ aCells]]}]
 ]
```

![0vou8cc6ekx82](img/0vou8cc6ekx82.png)

### Compute Nearest Neighbors graph

Create a function to find the nearest large cities to a given position in the U.S.:

```mathematica
nc = Nearest[Values[aCells] -> Keys[aCells], 
  DistanceFunction -> (EuclideanDistance[#1["Center"], #2[
       "Center"]] &)]
```

![14q4els5zuwd2](img/14q4els5zuwd2.png)

```mathematica
lsDistances = 
  Select[Flatten@
    DistanceMatrix[Values[#["Center"] & /@ aCells]], # > 0 &];
ResourceFunction["RecordsSummary"][lsDistances]
```

![04dbdi7xkst0c](img/04dbdi7xkst0c.png)

Identify outlier(s) and drop them:

```mathematica
pos = Select[
  nc[#, {6, 1.1*Min[lsDistances]/Cos[\[Pi]/6.]}] & /@ aCells, 
  Length[#] == 1 &]

(*<|1 -> {1}|>*)
```

```mathematica
aCells = KeyDrop[aCells, Keys[pos]];
```

Re-assign ID’s:

```mathematica
aCells = AssociationThread[Range[Length[aCells]], Values[aCells]];
aCells = Association@
   KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];
```

```mathematica
grHexagonCellsNetwork = 
  NearestNeighborGraph[
   Keys[aCells], {7, Min[lsDistances]/Cos[\[Pi]/6.]}, 
   DistanceFunction -> (EuclideanDistance[aCells[#1]["Center"], 
       aCells[#2]["Center"]] &), 
   VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells], 
   VertexLabels -> "Name", ImageSize -> Large];
```

Verification plot:

```mathematica
If[TrueQ[renderGraphPlotsQ],
 Show[Graphics[{FaceForm[GrayLevel[0.9]], EdgeForm[Red], 
    Values[#["Cell"] & /@ aCells]}], grHexagonCellsNetwork, 
  ImageSize -> Large]
 ]
```

![196gh7308mnkw](img/196gh7308mnkw.png)

## Assign traffic

Here we use a simple heuristic: the traffic between two nodes is a certain fraction of the sum of the populations at those two nodes. The traffic fraction can be constant or seasonal (time dependent).

```mathematica
grHexagonCells = 
  Graph[DirectedEdge @@@ 
    Join[EdgeList[grHexagonCellsNetwork], 
     Reverse /@ EdgeList[grHexagonCellsNetwork]], 
   DirectedEdges -> True, 
   VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells], 
   ImageSize -> Large, VertexLabels -> Placed[Automatic, Center], 
   VertexSize -> .4];
If[TrueQ[renderGraphPlotsQ], grHexagonCells]
```

![06mxkletvfwyb](img/06mxkletvfwyb.png)

Here is the corresponding, heuristic traveling patterns matrix:

```mathematica
matHexagonCellsTraffic = 
 SparseArray[
  Map[(List @@ #) -> 
     trafficFraction*
      Mean[Map[aCells[#]["Population"] &, List @@ #]] &, 
   EdgeList[grHexagonCells]]]
```

![0xackkzn1ctje](img/0xackkzn1ctje.png)

Here we make the traveling patterns matrix time-dependent in order to be able to simulate quarantine scenarios:

```mathematica
If[quarantineStart < maxTime && quarantineTrafficFractionFactor != 1,
 matHexagonCellsTraffic = 
   matHexagonCellsTraffic*
    Piecewise[{{1, 
       t < quarantineStart}, {quarantineTrafficFractionFactor, 
       quarantineStart <= t <= quarantineStart + quarantineDuration}},
      1];
 ]
```

Here we make a constant traveling matrix and summarize it:

```mathematica
Block[{matTravel = Normal[matHexagonCellsTraffic] /. t -> 1.0},
 {ResourceFunction["RecordsSummary"][Flatten[matTravel], 
    "All elements"][[1]], 
  ResourceFunction["RecordsSummary"][
    Select[Flatten[matTravel], # > 0 &], "Non-zero elements"][[1]], 
  MatrixPlot[matTravel, ImageSize -> Medium]}
 ]
```

![0lyaokwj92mx3](img/0lyaokwj92mx3.png)

## Initial infected population

If the nodes with initial infected populations are not specified we simply use randomly selected graph nodes.

Find bottom graph nodes (to be used in the initial conditions below):

```mathematica
If[TrueQ[lsPatientZeroNodeInds === Automatic] || ! 
   VectorQ[lsPatientZeroNodeInds, IntegerQ],
 lsPatientZeroNodeInds = 
  RandomSample[Keys[TakeSmallest[#["Center"][[2]] & /@ aCells, 8]], 
   2]
 ]

(*{65, 78}*)
```

## Single-site “seed” model 

In this section we create a single-site model that is being replicated over the graph nodes. (The rates and initial conditions are replicated for all nodes.)

```mathematica
model1 = singleSiteModelFunc[t, "InitialConditions" -> True, 
   "RateRules" -> True, 
   "TotalPopulationRepresentation" -> "AlgebraicEquation", 
   "BirthsTerm" -> TrueQ[includeBirthsTermQ]];
```

Quarantine scenarios functions:

```mathematica
model1 =
  SetRateRules[model1,
   <|\[Beta][ISSP] -> 
     0.56*Piecewise[{{1, 
         t < quarantineStart}, {quarantineContactRateFactor, 
         quarantineStart <= t <= 
          quarantineStart + quarantineDuration}}, 1],
    \[Beta][INSP] -> 
     0.56*Piecewise[{{1, 
         t < quarantineStart}, {quarantineContactRateFactor, 
         quarantineStart <= t <= 
          quarantineStart + quarantineDuration}}, 1]|>];
```

Number of beds per 1000 people:

```mathematica
model1 = SetRateRules[model1, <|nhbr[TP] -> 8/1000|>];
```

## Main multi-site workflow

In this section we do the model extension and simulation over a the hexagonal cells graph and the corresponding constant traveling patterns matrix.

Here we scale the SEI2R model with the grid graph constant traveling matrix:

```mathematica
AbsoluteTiming[
 modelHexGermany = 
   ToSiteCompartmentsModel[model1, matHexagonCellsTraffic, 
    "MigratingPopulations" -> Automatic];
 ]

(*{1.25226, Null}*)
```

Change the initial conditions in the following way: 

- Pick initial population size to be the hexagonal cell population

- Make a (constant) populations vector

- At all sites -- except selected “patient 0” ones -- put the infected populations to zero; the selected “patient 0” sites have one severely symptomatic person

- Set the susceptible populations to be consistent with the total and infected populations.

```mathematica
lsRPopulations = 
  aCells[#]["Population"] & /@ 
   Range[Dimensions[matHexagonCellsTraffic][[1]]];
maxPopulation = Max[lsRPopulations];
sInds = Flatten[
   Position[Keys[aCells], #] & /@ lsPatientZeroNodeInds];
AbsoluteTiming[
 modelHexGermany =
   SetInitialConditions[
    modelHexGermany,
    Join[
     Join[
      AssociationThread[
       Through[GetPopulationSymbols[modelHexGermany, 
          "Total Population"][0]], lsRPopulations], 
      Association[Map[TP[#][0] -> lsRPopulations[[#]] &, sInds]]],
     Join[
      Association@
       Map[#[0] -> 0 &, 
        GetPopulationSymbols[modelHexGermany, 
         "Infected Severely Symptomatic Population"]], 
      Association[Map[ISSP[#][0] -> 1 &, sInds]]],
     Join[
      Association@
       Map[#[0] -> 0 &, 
        GetPopulationSymbols[modelHexGermany, 
         "Infected Normally Symptomatic Population"]], 
      Association[Map[INSP[#][0] -> 0 &, sInds]]],
     Join[
      AssociationThread[
       Through[GetPopulationSymbols[modelHexGermany, 
          "Susceptible Population"][0]], lsRPopulations], 
      Association[Map[SP[#][0] -> lsRPopulations[[#]] - 1 &, sInds]]]
     ]
    ];
 ]

(*{0.245838, Null}*)
```

Solve the system of ODE’s of the scaled model:

```mathematica
AbsoluteTiming[
 aSolHexGermany = Association@First@
     NDSolve[
      Join[
       modelHexGermany["Equations"] //. modelHexGermany["RateRules"], 
       modelHexGermany["InitialConditions"] //. 
        modelHexGermany["RateRules"]],
      GetStockSymbols[modelHexGermany, __ ~~ __],
      {t, 0, maxTime}
      ];
 ]

(*{28.5311, Null}*)
```

Number of solutions:

```mathematica
aSolHexGermany // Length

(*1452*)
```

Randomly sample the graph sites and display the solutions separately for each site in the sample:

```mathematica
If[TrueQ[renderSolultionsPlotsQ],
 Multicolumn[
  Table[
   Block[{aSol = 
      KeySelect[
       aSolHexGermany, ! MemberQ[{MLP, MHS}, Head[#]] && 
         MatchQ[#, _Symbol[i]] &]},
    Plot[Evaluate[Map[#[t] &, Values[aSol]]], {t, 0, maxTime}, 
     PlotRange -> All, GridLines -> All, PlotTheme -> "Scientific", 
     PlotLegends -> Keys[aSol], ImageSize -> 300]
    ], {i, RandomSample[Range[VertexCount[grHexagonCells]], UpTo[9]]}],
  3]
 ]
```

![0vx7782caocz3](img/0vx7782caocz3.png)

Display solutions of the first and last site:

```mathematica
If[TrueQ[renderSolultionsPlotsQ],
 Multicolumn[
  Table[
   Block[{aSol = 
      KeySelect[
       aSolHexGermany, ! MemberQ[{MLP, MHS}, Head[#]] && 
         MatchQ[#, _Symbol[i]] &]},
    Plot[Evaluate[Map[#[t] &, Values[aSol]]], {t, 0, maxTime}, 
     PlotRange -> All, GridLines -> All, PlotTheme -> "Scientific", 
     PlotLegends -> Keys[aSol], ImageSize -> 300]
    ], {i, Join[sInds, {VertexCount[grHexagonCells]}]}],
  3]
 ]
```

![19u0a911l8upe](img/19u0a911l8upe.png)

As expected from the graph structure, we can see in the first site plot that its total population is decreasing -- nobody is traveling to the first site. Similarly, we can see in the last site plot that its total population is increasing -- nobody leaves the last site.

### Graph evolution visualizations

We can visualize the spatial-temporal evolution of model’s populations using sequences of graphs. The graphs in the sequences are copies of the multi-site graph each copy having its nodes colored according to the populations in the solutions steps. 

Here is a sub-sequence for the total populations:

```mathematica
If[TrueQ[renderGraphicsOfSolultionsOverGraphQ],
 EvaluateSolutionsOverGraph[grHexagonCells, modelHexGermany, 
  "Total Population", aSolHexGermany, {1, maxTime, 24}, 
  "NodeSizeFactor" -> 2.5, "ColorScheme" -> "TemperatureMap", 
  "Legended" -> True, VertexLabels -> None, ImageSize -> 200]
 ]
```

Here is the “lost money evolution”:

```mathematica
If[TrueQ[renderGraphicsOfSolultionsOverGraphQ],
 EvaluateSolutionsOverGraph[grHexagonCells, 
  modelHexGermany, __ ~~ "Productivity", 
  aSolHexGermany, {1, maxTime, 24}, "NodeSizeFactor" -> 3, 
  "ColorScheme" -> "TemperatureMap", "Legended" -> True, 
  VertexLabels -> None, ImageSize -> 200]
 ]
```

Here is a sub-sequence for the sum of the infected populations:

```mathematica
If[TrueQ[renderGraphicsOfSolultionsOverGraphQ],
 EvaluateSolutionsOverGraph[grHexagonCells, 
  modelHexGermany, {"Infected Normally Symptomatic Population", 
   "Infected Severely Symptomatic Population"}, 
  aSolHexGermany, {1, maxTime, 24}, "NodeSizeFactor" -> 4, 
  "ColorScheme" -> "TemperatureMap", "Normalization" -> "ByVertex", 
  "Legended" -> True, VertexLabels -> None, ImageSize -> 200]
 ]
```

Here is a sub-sequence for the recovered population:

```mathematica
If[TrueQ[renderGraphicsOfSolultionsOverGraphQ],
 EvaluateSolutionsOverGraph[grHexagonCells, modelHexGermany, 
  "Recovered Population", aSolHexGermany, {1, maxTime, 24}, 
  "NodeSizeFactor" -> 4, "ColorScheme" -> "TemperatureMap", 
  "Normalization" -> "ByVertex", "Legended" -> True, 
  VertexLabels -> None, ImageSize -> 200]
 ]
```

Here is an animation of the sum of the infected populations:

```mathematica
If[TrueQ[renderSolutionAnimationQ],
 Block[{stocks = {"Infected Normally Symptomatic Population", 
     "Infected Severely Symptomatic Population"}, 
   colorScheme = "TemperatureMap", timeStep = 3},
  lsAninmationPlots = 
   ListAnimate[
    EvaluateSolutionsOverGraph[grHexagonCells, modelHexGermany, 
     stocks, aSolHexGermany, {1, maxTime, timeStep}, 
     "Normalization" -> "ByVertex", "NodeSizeFactor" -> 2.5, 
     "ColorScheme" -> colorScheme, VertexLabels -> None, 
     ImageSize -> 400]];
  Legended[lsAninmationPlots, BarLegend[colorScheme]
   ]
  ]
 ]
```

```mathematica
(*AbsoluteTiming[
Export[FileNameJoin[{NotebookDirectory[],"Germany-hexagonal-grid-\
network-traveling-IP-simulation.gif"}],lsAninmationPlots]
]*)
```

## Summary statistics

```mathematica
ResourceFunction["RecordsSummary"][
 MinMax /@ 
  EvaluateSolutionsOverGraphVertexes[grHexagonCells, modelHexGermany, 
   "Recovered Population", aSolHexGermany, {1, maxTime, 1}], 
 Thread -> True]
```

![17w8kdvkcme40](img/17w8kdvkcme40.png)

### Infected Population

Single-site profiles:

```mathematica
If[TrueQ[renderSolultionsPlotsQ], 
 ListLinePlot[#, PlotTheme -> "Detailed", PlotRange -> All] & /@ 
  RandomSample[
   EvaluateSolutionsOverGraphVertexes[grHexagonCells, 
    modelHexGermany, {"Infected Normally Symptomatic Population", 
     "Infected Severely Symptomatic Population"}, 
    aSolHexGermany, {1, maxTime, 4}], 12]
 ]
```

![15e3jv8hgd7dl](img/15e3jv8hgd7dl.png)

Total (of all sites) profile: 

```mathematica
If[TrueQ[renderSolultionsPlotsQ], 
 ListLinePlot[
  Total[Values[
    EvaluateSolutionsOverGraphVertexes[grHexagonCells, 
     modelHexGermany, {"Infected Normally Symptomatic Population", 
      "Infected Severely Symptomatic Population"}, 
     aSolHexGermany, {1, maxTime, 1}]]], PlotTheme -> "Detailed", 
  PlotRange -> All]
 ]
```

![028ra0l91cf8s](img/028ra0l91cf8s.png)

### Recovered Population

Total of the Recovered Population:

```mathematica
If[TrueQ[renderSolultionsPlotsQ],
 ListLinePlot[
  Total[Values[
    EvaluateSolutionsOverGraphVertexes[grHexagonCells, 
     modelHexGermany, "Recovered Population", 
     aSolHexGermany, {1, maxTime, 1}]]], PlotTheme -> "Detailed", 
  PlotRange -> All]
 ]
```

![0n85uaj7rrjmz](img/0n85uaj7rrjmz.png)

### Deceased Infected Population

Total of the Deceased Infected Population:

```mathematica
If[TrueQ[renderSolultionsPlotsQ] && 
  MemberQ[Union[Values[modelHexGermany["Stocks"]]], 
   "Deceased Infected Population"],
 ListLinePlot[
  Total[Values[
    EvaluateSolutionsOverGraphVertexes[grHexagonCells, 
     modelHexGermany, "Deceased Infected Population", 
     aSolHexGermany, {1, maxTime, 1}]]], PlotTheme -> "Detailed", 
  PlotRange -> All]
 ]
```

![07zxtfzrckzsh](img/07zxtfzrckzsh.png)

Ratio of Total of the Deceased Infected and Total Population:

![1hov12cbfynej](img/1hov12cbfynej.png)

![0lt2cktidnwk6](img/0lt2cktidnwk6.png)

### Hospitalized Population

Total of the Hospitalized Population:

```mathematica
If[TrueQ[renderSolultionsPlotsQ] && 
  MemberQ[Union[Values[modelHexGermany["Stocks"]]], 
   "Hospitalized Population"],
 ListLinePlot[
  Total[Values[
    EvaluateSolutionsOverGraphVertexes[grHexagonCells, 
     modelHexGermany, "Hospitalized Population", 
     aSolHexGermany, {1, maxTime, 1}]]], PlotTheme -> "Detailed", 
  PlotRange -> All]
 ]
```

![03pfejxkf9ebu](img/03pfejxkf9ebu.png)

## Export

```mathematica
Union[Values[modelHexGermany["Stocks"]]]

(*{"Deceased Infected Population", "Exposed Population", "Hospital \
Beds", "Hospitalized Population", "Infected Normally Symptomatic \
Population", "Infected Severely Symptomatic Population", "Money for \
Hospital Services", "Money of Lost Productivity", "Recovered \
Population", "Susceptible Population", "Total Population"}*)
```

```mathematica
AbsoluteTiming[
 aSolData = 
   Association@
    Map[# -> 
       Round[EvaluateSolutionsOverGraphVertexes[grHexagonCells, 
         modelHexGermany, #, aSolHexGermany, {1, maxTime, 1}], 
        0.001] &, Union[Values[modelHexGermany["Stocks"]]]];
 ]

(*{1.08641, Null}*)
```

```mathematica
dsSolData = ConvertSolutions[aSolData, "Array"];
RandomSample[dsSolData, 4]

(*{{"Total Population", 72, 353, 
  591838.}, {"Infected Severely Symptomatic Population", 97, 165, 
  103258.}, {"Total Population", 72, 14, 
  601322.}, {"Infected Normally Symptomatic Population", 61, 213, 
  125450.}}*)
```

```mathematica
timeStamp = StringReplace[DateString["ISODateTime"], ":" -> "."]

(*"2020-03-22T18.50.27"*)
```

```mathematica
If[! StringQ[exportFileNamePrefix], 
  exportFileNamePrefix = "WirVsVirus-"];
```

```mathematica
aParams = <|
   "singleSiteModelFunc" -> singleSiteModelFunc,
   "cellRadius" -> cellRadius,
   "trafficFraction" -> trafficFraction,
   "includeBirthsTermQ" -> includeBirthsTermQ,
   "quarantineStart" -> quarantineStart,
   "quarantineDuration" -> quarantineDuration,
   "quarantineContactRateFactor" -> quarantineContactRateFactor,
   "quarantineTrafficFractionFactor" -> 
    quarantineTrafficFractionFactor,
   "exportFileNamePrefix" -> exportFileNamePrefix
   |>;
dsParams = List @@@ Normal[aParams];
```

```mathematica
If[TrueQ[exportSolutionsQ],
 Export[FileNameJoin[{NotebookDirectory[], 
    StringJoin[exportFileNamePrefix, "GSTEM-Parameters-", timeStamp, 
     ".csv"]}], Prepend[dsParams, {"Parameter", "Value"}], "CSV"];
 Export[FileNameJoin[{NotebookDirectory[], 
    StringJoin[exportFileNamePrefix, "GSTEM-Solutions-", timeStamp, 
     ".csv"]}], 
  Prepend[dsSolData, {"Stock", "Node", "Time", "Value"}], "CSV"];
 ]
```

## References

### Articles

[AA1] Anton Antonov, ["Coronavirus propagation modeling considerations"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/Documents/Coronavirus-propagation-modeling-considerations.md), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AA2] Anton Antonov, ["Basic experiments workflow for simple epidemiological models"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/Documents/Basic-experiments-workflow-for-simple-epidemiological-models.md), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AA3] Anton Antonov, ["Scaling of Epidemiology Models with Multi-site Compartments"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/Documents/Scaling-of-epidemiology-models-with-multi-site-compartments.md), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

### Repositories, packages, data

[WRI1] Wolfram Research, Inc., ["Epidemic Data for Novel Coronavirus COVID-19"](https://www.wolframcloud.com/obj/resourcesystem/published/DataRepository/resources/Epidemic-Data-for-Novel-Coronavirus-COVID-19), [WolframCloud](https://www.wolframcloud.com).

[WRI2] Wolfram Research, Inc., [Germany city data records](https://github.com/antononcube/SystemModeling/blob/master/Data/dfGermanyCityRecords.csv), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAr1] Anton Antonov, [Coronavirus propagation dynamics project](https://github.com/antononcube/SystemModeling/tree/master/Projects/Coronavirus-propagation-dynamics), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp1] Anton Antonov, ["Epidemiology models Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m), (2020), [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp2] Anton Antonov, ["Epidemiology models modifications Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelModifications.m), (2020), [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp3] Anton Antonov, ["Epidemiology modeling visualization functions Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelingVisualizationFunctions.m), (2020), [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp4] Anton Antonov, ["System dynamics interactive interfaces functions Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/WL/SystemDynamicsInteractiveInterfacesFunctions.m), (2020), [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling).

### Project management files

[AAo1] Anton Antonov, [WirVsVirus-Hackathon-work-plan.org](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/org/WirVsVirus-Hackathon-work-plan.org), (2020), [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAo2] Anton Antonov, [WirVsVirus-hackathon-Geo-spatial-temporal-model-mind-map](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/org/WirVsVirus-hackathon-Geo-spatial-temporal-model-mind-map.pdf), (2020), [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling).
