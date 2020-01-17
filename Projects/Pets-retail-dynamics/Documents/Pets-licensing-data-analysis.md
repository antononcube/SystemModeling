# Pets licensing data analysis

Anton Antonov
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)
[SystemModeling at GitHub](https://github.com/antononcube/SystemModeling)
January 2020

#### Introduction

This notebook / document provides ground data analysis used to make or confirm certain modeling conjectures and assumptions of a Pets Retail Dynamics Model (PRDM), [ [AA1]](https://github.com/antononcube/SystemModeling/tree/master/Projects/Pets-retail-dynamics). Seattle pets licensing data is used, [[SOD2](https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data)].

We want to provide answers to the following questions.

   + Does the Pareto principle manifests for pets breeds?

   + Does the Pareto principle manifests for ZIP codes?

   + Is there an upward trend for becoming a pet owner?

All three questions have positive answers, assuming the retrieved data, [[SOD2](https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data)], is representative. See the last section for an additional discussion.

We also discuss pet adoption simulations that are done using Quantile Regression, [AA2, AAp1].

This notebook/document is part of the [SystemsModeling at GitHub](https://github.com/antononcube/SystemModeling) project ["Pets retail dynamics", [AA1].](https://github.com/antononcube/SystemModeling/tree/master/Projects/Pets-retail-dynamics)

#### Data

The pet licensing data was taken from this page:  [“Seattle Pet Licenses”](https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data) , [https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data](https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data) . 

The ZIP code coordinates data was taken from a GitHub repository,  ["US Zip Codes from 2013 Government Data", https://gist.github.com/erichurst/7882666 .](https://gist.github.com/erichurst/7882666)

##### Animal licenses

```mathematica
dsPetLicenses=ResourceFunction["ImportCSVToDataset"]["~/Datasets/Seattle/Seattle_Pet_Licenses.csv"]
```

![image-ea876d98-0d9b-4cd9-85b3-fda700b8bbb3](Diagrams/Pets-licensing-data-analysis/image-ea876d98-0d9b-4cd9-85b3-fda700b8bbb3.png)

Convert “Licence Issue Date” values into DateObjects.

```mathematica
dsPetLicenses=dsPetLicenses[All,Prepend[#,"DateObject"\[Rule]DateObject[{#\[LeftDoubleBracket]1\[RightDoubleBracket],{"Month","Day","Year"}}]]&];
```

###### Summary

```mathematica
ResourceFunction["RecordsSummary"][dsPetLicenses]
```

![image-8b9d9b09-962c-4161-8486-087bb8b1bf2c](Diagrams/Pets-licensing-data-analysis/image-8b9d9b09-962c-4161-8486-087bb8b1bf2c.png)

###### Keep dogs and cats only

Since the number of animals that are not cats or dogs is very small we remove them from the data in order to produce more concise statistics.

```mathematica
dsPetLicenses=dsPetLicenses[Select[MemberQ[{"Cat","Dog"},#Species]&]];
    Dimensions[dsPetLicenses]

(*{49191,8}*)
```

##### ZIP code geo-coordinates

```mathematica
dsZIPCodes=ImportCSVToDataset["~/Datasets/USAZipCodes/US-Zip-Codes-from-2013-Government-Data.csv"];
    Dimensions[dsZIPCodes]

(*{33144,3}*)
```

```mathematica
aZipLatLon=Association[Normal[Query[#ZIP\[Rule]{#LAT,#LON}&]/@dsZIPCodes]];
    aZipLatLon=KeyMap[ToString,aZipLatLon];
    Length[aZipLatLon]

(*33144*)
```

###### Summary

```mathematica
ResourceFunction["RecordsSummary"][dsZIPCodes]
```

![image-20da68a4-d1e4-4c24-9622-ec6974c0d72a](Diagrams/Pets-licensing-data-analysis/image-20da68a4-d1e4-4c24-9622-ec6974c0d72a.png)

```mathematica
ResourceFunction["RecordsSummary"][aZipLatLon,Thread\[Rule]True]
```

![image-0dff52f8-6a51-4b61-be10-cd401172c34e](Diagrams/Pets-licensing-data-analysis/image-0dff52f8-6a51-4b61-be10-cd401172c34e.png)

#### Pareto principle adherence

In this section we apply to the Pareto principle statistic in order to see does the Pareto principle manifests over the different columns of the pet licensing data.

##### Breeds

We see a typical Pareto principle adherence for both dog breeds and cat breeds. For example, $20$% of the dog breeds correspond to $80$% of all registered dogs. 

Note the number of unique cat breeds is 4 times smaller than the number of unique dog breeds.

```mathematica
focusColumnName="Primary Breed";
```

```mathematica
dsQuery=Query[GroupBy[#Species&],GroupBy[#[focusColumnName]&],Length]@dsPetLicenses;
    dsQuery=Dataset[ReverseSort/@Normal[dsQuery]]
```

![image-833188a8-e768-4e14-815e-ccf490f5ac74](Diagrams/Pets-licensing-data-analysis/image-833188a8-e768-4e14-815e-ccf490f5ac74.png)

```mathematica
KeyValueMap[ResourceFunction["ParetoPrinciplePlot"][Values[#2],PlotLabel\[Rule]Row[{#1,Spacer[3],focusColumnName}],ImageSize\[Rule]Medium,opts]&,Normal[dsQuery]]
```

![image-38f833a7-7567-40b7-852b-81c5b655b980](Diagrams/Pets-licensing-data-analysis/image-38f833a7-7567-40b7-852b-81c5b655b980.png)

##### Animal names

We see a typical Pareto principle adherence for the frequencies of the pet names. For dogs, $10$% of the unique names correspond to $\text{$\$$Failed}$% of the pets.

```mathematica
focusColumnName="Animal's Name";
```

```mathematica
dsQuery=Query[GroupBy[#Species&],GroupBy[#[focusColumnName]&],Length]@dsPetLicenses;
    dsQuery=Dataset[ReverseSort/@Normal[dsQuery]]
```

![image-c157c400-62d9-4b82-bdb1-91a7d66399f4](Diagrams/Pets-licensing-data-analysis/image-c157c400-62d9-4b82-bdb1-91a7d66399f4.png)

```mathematica
KeyValueMap[ResourceFunction["ParetoPrinciplePlot"][Values[#2],PlotLabel\[Rule]Row[{#1,Spacer[3],focusColumnName}],ImageSize\[Rule]Medium,opts]&,Normal[dsQuery]]
```

![image-a90843ee-1fa7-4821-af6a-5b8703eab134](Diagrams/Pets-licensing-data-analysis/image-a90843ee-1fa7-4821-af6a-5b8703eab134.png)

##### Zip codes

We see typical -- even exaggerated -- manifestation of the Pareto principle over ZIP codes of the registered pets.

```mathematica
focusColumnName="ZIP Code";
```

```mathematica
dsQuery=Query[GroupBy[#Species&],GroupBy[#[focusColumnName]&],Length]@dsPetLicenses;
    dsQuery=Dataset[ReverseSort/@Normal[dsQuery]];
```

```mathematica
KeyValueMap[ResourceFunction["ParetoPrinciplePlot"][Values[#2],PlotLabel\[Rule]Row[{#1,Spacer[3],focusColumnName}],ImageSize\[Rule]Medium,opts]&,Normal[dsQuery]]
```

![image-4f0afaeb-f6a6-4d45-ad6e-383f84ed10fc](Diagrams/Pets-licensing-data-analysis/image-4f0afaeb-f6a6-4d45-ad6e-383f84ed10fc.png)

#### Geo-distribution

In this section we visualize the pets licensing geo-distribution with geo-histograms.

```mathematica
city=Entity["City",{"Seattle","Washington","UnitedStates"}];
    GeoPosition[city]

(*GeoPosition[{47.6205,-122.351}]*)
```

##### Both cats and dogs

```mathematica
lsCoords=Map[If[KeyExistsQ[aZipLatLon,#],aZipLatLon[#],Nothing]&,Select[ToString/@Normal[dsPetLicenses[All,"ZIP Code"]],StringQ[#]&&StringLength[#]>=5&]];
```

```mathematica
GeoHistogram[lsCoords,GeoCenter\[Rule]city,GeoRange\[Rule]Quantity[20,"Miles"],PlotLegends\[Rule]Automatic,ColorFunction\[Rule](Hue[2/3,2/3,1-#]&),opts]
```

![image-b5a2acab-0035-447f-aedd-42a5ca41f961](Diagrams/Pets-licensing-data-analysis/image-b5a2acab-0035-447f-aedd-42a5ca41f961.png)

##### Cats and dogs separate

```mathematica
lsCoords=Map[If[KeyExistsQ[aZipLatLon,#],aZipLatLon[#],Nothing]&,Select[ToString/@Normal[dsPetLicenses[Select[#Species\[Equal]"Dog"&],"ZIP Code"]],StringQ[#]&&StringLength[#]>=5&]];
    gr1=GeoHistogram[lsCoords,GeoCenter\[Rule]city,GeoRange\[Rule]Quantity[20,"Miles"],PlotLegends\[Rule]Automatic,ColorFunction\[Rule](Hue[2/3,2/3,1-#]&),opts];
```

```mathematica
lsCoords=Map[If[KeyExistsQ[aZipLatLon,#],aZipLatLon[#],Nothing]&,Select[ToString/@Normal[dsPetLicenses[Select[#Species\[Equal]"Cat"&],"ZIP Code"]],StringQ[#]&&StringLength[#]>=5&]];
    gr2=GeoHistogram[lsCoords,GeoCenter\[Rule]city,GeoRange\[Rule]Quantity[20,"Miles"],PlotLegends\[Rule]Automatic,ColorFunction\[Rule](Hue[2/3,2/3,1-#]&),opts];
```

```mathematica
ResourceFunction["GridTableForm"][{gr1,gr2},TableHeadings\[Rule]{"Dogs","Cats"},Background\[Rule]White]
```

![image-5a06739b-d88c-49c4-9886-ea2c64b72afb](Diagrams/Pets-licensing-data-analysis/image-5a06739b-d88c-49c4-9886-ea2c64b72afb.png)

##### Pet stores

In this subsection we show the distribution of pet stores (in Seattle). 

It is better instead of image retrieval we should show corresponding geo-markers in the geo-histograms above. (This is not considered that important in the first version of this notebook/document.)

```mathematica
(*WebImage["https://www.google.com/maps/search/pet+stores+in+Seattle,+WA/@47.6326975,-122.4227211,12.05z"]*)
```

#### Time series

In this section we visualize the time series corresponding to the pet registrations.

##### Time series objects

Here we make time series objects:

```mathematica
dsQuery=Query[GroupBy[#Species&],GroupBy[#DateObject&],Length]@dsPetLicenses;
    aTS=TimeSeries/@(List@@@Normal[#]&/@Normal[dsQuery])
```

![image-962fa7ef-5050-4e22-b58a-9cd42f6b96c6](Diagrams/Pets-licensing-data-analysis/image-962fa7ef-5050-4e22-b58a-9cd42f6b96c6.png)

##### Time series plots of all registrations

Here are time series plots corresponding to all registrations:

```mathematica
DateListPlot[#,opts]&/@aTS
```

![image-7f3b2bb5-6546-4a43-a40c-65ccfc414132](Diagrams/Pets-licensing-data-analysis/image-7f3b2bb5-6546-4a43-a40c-65ccfc414132.png)

##### Time series plots of most recent registrations

It is an interesting question why the number of registrations is much higher in volume and frequency in the years 2018 and later.

```mathematica
DateListPlot[TimeSeriesWindow[#,{{2017,1,1},{2020,1,1}}],opts]&/@aTS
```

![image-79abc7dd-3af1-4738-a344-95da51644b51](Diagrams/Pets-licensing-data-analysis/image-79abc7dd-3af1-4738-a344-95da51644b51.png)

##### Upward trend

Here we apply both Linear Regression and Quantile Regression:

```mathematica
QRMonUnit[TimeSeriesWindow[#,{{2018,1,1},{2020,1,1}}]]\[DoubleLongRightArrow]
    QRMonLeastSquaresFit[{1,x}]\[DoubleLongRightArrow]
    QRMonQuantileRegressionFit[4,0.5]\[DoubleLongRightArrow]
    QRMonDateListPlot[opts,"Echo"->False]\[DoubleLongRightArrow]
    QRMonTakeValue&/@aTS
```

![image-490a024e-09c7-4307-9181-0c95a88fefc5](Diagrams/Pets-licensing-data-analysis/image-490a024e-09c7-4307-9181-0c95a88fefc5.png)

We can see that there is clear upward trend for both dogs and cats.

#### Quantile regression application

In this section we investigate the possibility to simulate the pet adoption rate. We plan to use simulations of the pet adoption rate in PRDM.

We do that using the software monad QRMon, [AAp1]. A list of steps follows.

   + Split the time series into windows corresponding to the years 2018 and 2019.

   + Find the difference between the two years.

   + Apply Quantile Regression to the difference using a reasonable grid of probabilities.

   + Simulate the difference.

   + Add the simulated difference to year 2019.

##### Simulation

In this sub-section we simulate the differences between the time series for 2018 and 2019, then we add the simulated difference to the time series of the year 2019.

```mathematica
ts1=TimeSeriesResample[TimeSeriesWindow[aTS\[LeftDoubleBracket]1\[RightDoubleBracket],{{2018,1,1},{2019,1,1}}],"Day"];
    ts1["Path"]\[LeftDoubleBracket]All,2\[RightDoubleBracket];
```

```mathematica
ts2=TimeSeriesResample[TimeSeriesWindow[aTS\[LeftDoubleBracket]1\[RightDoubleBracket],{{2019,1,1},{2020,1,1}}],"Day"];
    ts2["Path"]\[LeftDoubleBracket]All,2\[RightDoubleBracket];
```

```mathematica
ts3=TimeSeries[Transpose[{ts1["Path"]\[LeftDoubleBracket]All,1\[RightDoubleBracket],ts2["Path"]\[LeftDoubleBracket]All,2\[RightDoubleBracket]-ts1["Path"]\[LeftDoubleBracket]All,2\[RightDoubleBracket]}]]
```

![image-dfdd4c7f-24b6-4500-8650-247a69c51b60](Diagrams/Pets-licensing-data-analysis/image-dfdd4c7f-24b6-4500-8650-247a69c51b60.png)

```mathematica
qrObj=
    QRMonUnit[ts3]\[DoubleLongRightArrow]
    QRMonEchoDataSummary\[DoubleLongRightArrow]
    QRMonQuantileRegression[20,Join[Range[0.1,0.9,0.1],{0.03,0.93}],InterpolationOrder\[Rule]2]\[DoubleLongRightArrow]
    QRMonDateListPlot[opts];
```

![image-3e02ea3d-59c3-4d7a-948d-7f16c5346af9](Diagrams/Pets-licensing-data-analysis/image-3e02ea3d-59c3-4d7a-948d-7f16c5346af9.png)

![image-5013d1ae-5d75-437a-b46a-a99c83e420fd](Diagrams/Pets-licensing-data-analysis/image-5013d1ae-5d75-437a-b46a-a99c83e420fd.png)

```mathematica
qrObj=
    qrObj\[DoubleLongRightArrow]
    QRMonEchoFunctionContext[DateListPlot[#data,PlotLabel\[Rule]"Original data",opts]&]\[DoubleLongRightArrow]
    QRMonSimulate[ts2["Path"]//Length]\[DoubleLongRightArrow]
    QRMonEchoFunctionValue[DateListPlot[#,PlotLabel\[Rule]"Simulated data",opts]&];
```

![image-176bfce1-4a9e-407b-ac96-1e1a12ea68d2](Diagrams/Pets-licensing-data-analysis/image-176bfce1-4a9e-407b-ac96-1e1a12ea68d2.png)

![image-7c613725-ed83-45b3-bf83-295304c9a8cb](Diagrams/Pets-licensing-data-analysis/image-7c613725-ed83-45b3-bf83-295304c9a8cb.png)

Take the simulated time series difference:

```mathematica
tsSimDiff=TimeSeries[qrObj\[DoubleLongRightArrow]QRMonTakeValue];
```

Add the simulated time series difference to year 2019, clip the values less than zero, shift the result to 2020:

```mathematica
tsSim=MapThread[{#1\[LeftDoubleBracket]1\[RightDoubleBracket],#1\[LeftDoubleBracket]2\[RightDoubleBracket]+#2\[LeftDoubleBracket]2\[RightDoubleBracket]}&,{ts2["Path"],tsSimDiff["Path"]}];
    tsSim\[LeftDoubleBracket]All,2\[RightDoubleBracket]=Clip[tsSim\[LeftDoubleBracket]All,2\[RightDoubleBracket],{0,Max[tsSim\[LeftDoubleBracket]All,2\[RightDoubleBracket]]}];
    tsSim=TimeSeriesShift[TimeSeries[tsSim],Quantity[365,"Days"]];
    DateListPlot[tsSim,opts]
```

![image-5fba8275-2ff2-4c59-9e31-34d3fbf4eaf5](Diagrams/Pets-licensing-data-analysis/image-5fba8275-2ff2-4c59-9e31-34d3fbf4eaf5.png)

##### Plot all years together

```mathematica
DateListPlot[{ts1,ts2,tsSim},opts,PlotLegends\[Rule]{2018,2019,2020}]
```

![image-7d92dbf0-962e-4e84-b0f5-35e22952d5aa](Diagrams/Pets-licensing-data-analysis/image-7d92dbf0-962e-4e84-b0f5-35e22952d5aa.png)

#### Discussion

This section has subsections that correspond to additional discussion questions. Not all questions are answered, the plan is to progressively answer the questions with the subsequent versions of the this notebook / document.

###### □ Too few pets

The number of registered pets seems too few. Seattle is a large city with more than $600000$ citizens; approximately $50$% of the USA households have dogs; hence the registered pets are too few ($\text{$\$$Failed}$). 

###### □ Why too few pets?

Seattle is a high tech city and its citizens are too busy to have pets?

Most people do not register their pets? (Very unlikely if they have used veterinary services.)

Incomplete data?

###### Adoption rate

Can we tell apart the adoption rates of pet-less people and people who already have pets?

#### Preliminary definitions

```mathematica
opts=Sequence@@{PlotRange\[Rule]All,ImageSize\[Rule]Medium,PlotTheme\[Rule]"Detailed"};
```

```mathematica
Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]
```

#### References

[AA1] Anton Antonov, [Pets retail dynamics project](https://github.com/antononcube/SystemModeling/tree/master/Projects/Pets-retail-dynamics), 2020, [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).
      URL: [https://github.com/antononcube/SystemModeling/tree/master/Projects/Pets-retail-dynamics](https://github.com/antononcube/SystemModeling/tree/master/Projects/Pets-retail-dynamics) .

[AA2] Anton Antonov, [A monad for Quantile Regression workflows](https://mathematicaforprediction.wordpress.com/2018/08/01/a-monad-for-quantile-regression-workflows/), (2018), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

[AAp1] Anton Antonov, [Monadic Quantile Regression Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m), (2018), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
          URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m) .

[SOD1] Seattle Open Data,  [“Seattle Pet Licenses”](https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data) , [https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data](https://data.seattle.gov/Community/Seattle-Pet-Licenses/jguv-t9rb/data) . 

#### Export

```mathematica
SetDirectory[NotebookDirectory[]]

(*"/Volumes/Macintosh HD/Users/antonov/SystemModeling/Projects/Pets-retail-dynamics/Documents"*)
```

```mathematica
Needs["M2MD`"]
```

```mathematica
EvaluationNotebook[]
```

![image-5fb95a49-d106-4481-b879-7bb67a633962](Diagrams/Pets-licensing-data-analysis/image-5fb95a49-d106-4481-b879-7bb67a633962.png)

```mathematica
Options[MDExport]

(*{"ImagesExportURL"\[Rule]Automatic,"ImagesFetchURL"\[Rule]"Relative","IgnoredStyles"\[Rule]None}*)
```

```mathematica
SeedRandom[2323]
```

```mathematica
MDExport["Pets-licensing-data-analysis.md",EvaluationNotebook[]]
```

[//]: # ($Failed)

[//]: # ($Failed)

[//]: # ($Failed)

[//]: # ($Failed)

[//]: # ($Failed)

[//]: # ($Failed)

[//]: # ($Failed)

```mathematica
SystemOpen["Pets-licensing-data-analysis.md"]
```