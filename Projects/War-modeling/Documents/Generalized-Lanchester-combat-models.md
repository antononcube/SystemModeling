# Generalized Lanchester combat models

**Version 1.0**

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
[SystemModeling at GitHub](https://github.com/antononcube/SystemModeling)   
October 2023

## Introduction

In this notebook we present generalized [Lanchester combat models](https://en.wikipedia.org/wiki/Lanchester%27s_laws) and use one of them for simulation and calibration examples with data of the [Battle of Iwo Jima](https://en.wikipedia.org/wiki/Battle_of_Iwo_Jima).

**Remark:** The content of this notebook is intentionally theoretically fundamental, but short and simple both mathematically and code-wise. 
Author's intent is to develop similar computational workflows for (1) field battles, like the [Normandy campaign](https://en.wikipedia.org/wiki/Operation_Overlord) during Word War II, 1944, or (2) urban warfare, like [Second Battle of Fallujah](https://en.wikipedia.org/wiki/Second_Battle_of_Fallujah), Iraq 2009, or [Battle of Bakhmut](https://en.wikipedia.org/wiki/Battle_of_Bakhmut), Ukraine 2023.

The main computational workflows are done with a 
[System Dynamics (SD)](https://en.wikipedia.org/wiki/System_dynamics) 
software monad implemented in the paclet 
["MonadicSystemDynamics"](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/MonadicSystemDynamics/), [AAp2]. 
That paclet is based on the monad implementation presented in 
["A monad for Epidemiologic Compartmental Modeling Workflows"](https://mathematicaforprediction.wordpress.com/2021/01/02/epidemiology-compartmental-modeling-monad/), [AA1]. 
The functions provided by the paclet [AAp2] have the prefix "SDMon", which stands for "**S**ystem **D**ynamics **Mon**ad".

### Why use the Battle of Iwo Jima in examples?

The examples below use the Battle of Iwo Jima because that is convenient both data-wise and mathematics-wise. Here are our reasons:

- The battle is important for the USA military, hence well documented and used in multiple contexts.

    - See, for example, mathematical articles like [JE1] and [RS1].

- (Relatively) well curated data can be found. Like:

    - Sizes of the military forces

    - Battle duration

- There is no need to:

    - Take care of negative stocks

    - Simulate "will to fight" -- Japanese soldiers fought to the last one

        - Japanese Prisoners Of War (POWs) became POWs because they were found unconscious...

Here is the invasion map of Iwo Jima, prepared in February 1945, [DR1]:

<!-- ![](./Diagrams/Generalized-Lanchester-combat-models/Battle-of-Iwo-Jima-Map.png) -->

<img src="./Diagrams/Generalized-Lanchester-combat-models/Battle-of-Iwo-Jima-Map.png" width="500"/>

### Notebook structure

- **Generalized model and variants**
  Main theory.

- **SDMon Model**
  Programmer's version.

- **Direct model simulation**
  Using the Battle of Iwo Jima data and related pre-computed rates.

- **Calibration**
  We can get the theoretically computed rates by using numerics!

- **Future plans**
  Make models, not war.

------

## Generalized models

This section presents a translation to English of  introductory paragraphs of [NM1]. (The same general model and breakdown is presented in [AS1].)

In the most general form, the Lanchester models can be described by the by the equation:

$$
\begin{align}
\frac{\partial x}{\partial t} &= ax + bxy + cy + d, \\
\frac{\partial y}{\partial t} &= ey + fxy + gx + h
\end{align}
$$

where:

- $a$ and $e$ define the rate of non-combat losses

- $b$ and $f$ define the rate of losses due to exposure to area targets

- $c$ and $g$  are losses due to forward enemy exposure

- $d$ and $h$ are approaching or retreating reserves

To determine the casualties of wars, actual or potential, the following four models are of greatest importance.

#### **1.** ***Lanchester*** proper model (only the coefficients $b$ and $f$ are available)

In this case:

- The number of casualties is proportional to the number of encounters between individuals.

- The number of encounters between individuals of the opposing sides is proportional to the number of victims.

- The number of victims is proportional to the number of encounters between individuals of the opposing parties.

    -  Product of the number of parties: $x \times  y$.

- This interaction is most relevant when the two sides are located in a common territory:

    - Guerrilla warfare, repression, enmity between two ethnic groups, etc.

#### **2.** ***Osipov*** model (coefficients $a$ and $e$)

- The number of victims is proportional to the number of the opposing side.

- This could be a classic military engagement where the two sides are in contact only on the front lines.

#### **3.** ***Peterson*** model (coefficients $a$ and $e$)

- The number of casualties is determined by the size of one's side.

- This could be a model of the Cold War, where the more of their submarines are on alert, the more of them die.

#### **4.** ***Brackney*** model (coefficients $a$ and $f$ or $b$ and $e$)

- The casualties of one side is proportional to the number of encounters and the other to the number of its opponent.

- The model was inspired of the Vietnam War and describes quite satisfactory.

    -  I.e. a conflict in which one side is engaged in classical warfare and the other in guerrilla warfare.

--------

## Model "rigidness"

The simplest, with Osipov terms only, generalized Lanchester model is:

$$
\begin{align}
x'(t)=-b y(t), \\
y'(t)=-a x(t).
\end{align}
$$

That model is a "rigid model" that admits an exact solution. (See Arnold's book "«Rigid» and «soft» mathematical models", [VA1].)

Here is the solution:

$$\frac{d x}{d y}=\frac{b y(t)}{a x(t)}$$

$$a x(t) d x =b y(t) d y$$

$$a x(t) ^2- b y(t)^2\text{==}\text{const}$$.

The evolution of the number of soldiers of armies $x$ and $y$ occurs along the hyperbola given by the equation $a x(t) ^2- b y(t)^2\text{==}\text{const}.$ The war evolves along that hyperbola, hence it depends on the starting point.

The corresponding manifold of hyperbolas is separated by the line $a x = b y$. If the starting point lies above this line, then the hyperbola finishes on the $y$-axis. This means that in the course of the war army $x$ decreases to zero and army $y$ wins.

**Remark:** Note that if the efficiency coefficients $a$ and $b$ are not constant then the equation $a x = b y$ determines a curve.

Here is an interactive interface that illustrates the properties of the simplest model:

```mathematica
Manipulate[
 ContourPlot[fX[a, x] x - fY[b, y] y, {x, 0, 10}, {y, 0, 10},
  PlotLegends -> Automatic,
  Contours -> Evaluate[Join[{{0, {Red, Thick}}}, DeleteCases[Table[{i, Blue}, {i, -100, 100, 0.25}], {0 | 0., _}]]],
  ContourShading -> False,
  FrameLabel -> {"x", "y"}],
 {{a, 0.1, "a, efficiency constant X:"}, 0, 1, 0.001, Appearance -> "Open"},
 {{b, 0.2, "b, efficiency constant Y:"}, 0, 1, 0.001, Appearance -> "Open"},
 {{fX, Sqrt[#1 Sqrt[#2]] &, "efficiency function X:"}, 
   {(Sqrt[#1] &) -> "\!\(\*SqrtBox[\(a\)]\)", 
     (Sqrt[#1 Sqrt[#2]] &) -> "\!\(\*SqrtBox[\(a \*SqrtBox[\(x\)]\)]\)"}, 
  ControlType -> SetterBar},
 {{fY, Sqrt[#1] &, "efficiency function Y:"}, 
   {(Sqrt[#1] &) -> "\!\(\*SqrtBox[\(b\)]\)", 
     (Sqrt[#1 Sqrt[#2]] &) -> "\!\(\*SqrtBox[\(b \*SqrtBox[\(y\)]\)]\)"}, 
  ControlType -> SetterBar}]
```

![17vlw0owd1g18](./Diagrams/Generalized-Lanchester-combat-models/17vlw0owd1g18.png)

------

## SDMon model

In this section we define the general model in a *simple programmatic* *form* using the paclet ["MonadicSystemDynamics"](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/MonadicSystemDynamics/), [AAp2].

**Remark:** A better programmatic form would have equation elements that prevent (army) stocks to become negative.

**Remark:** Compared to the previous section, below we follow "wordier" but self-explanatory notation that helps model understanding, evaluation, and enhancements.

Here are the stocks:

```mathematica
aStocks = <|
    X[t] -> "Soldiers of army X", 
    Y[t] -> "Soldiers of army Y" 
   |>;
```

Here are the rates:

```mathematica
aRates = <|
    fireEfficiencyX -> "Efficiency of force X ", 
    fireEfficiencyY -> "Efficiency of force Y", 
    fireEfficiencyXonY -> "Efficiency of force X on Y", 
    fireEfficiencyYonX -> "Efficiency of force Y on X", 
    growthX -> "Growth rate of force X due to new recruits", 
    growthY -> "Growth rate of force Y due to new recruits", 
    diseaseX -> "Disease rate in force X", 
    diseaseY -> "Disease rate in force Y", 
    fireFriendlyX -> "Friendly fire rate in force X", 
    fireFriendlyY -> "Friendly fire rate in force Y" 
   |>;
```

Here are rules that assign concrete values to the rates:

```mathematica
aRateRules = <|
     fireEfficiencyX -> 0.01, 
     fireEfficiencyY -> 0.015, 
     fireEfficiencyXonY -> 0.05, 
     fireEfficiencyYonX -> 0.03, 
     growthX -> 0, 
     growthY -> 0, 
     diseaseX -> 0.01, 
     diseaseY -> 0.01, 
     fireFriendlyX -> 0.001, 
     fireFriendlyY -> 0.001 
   |>;
```

Initial conditions (number of soldiers):

```mathematica
lsInitConds = {X[0] == 100000, Y[0] == 100000};
```

Lanchester (or Lotka-Volterra) interactions:

```mathematica
lotkaVolterraX = fireEfficiencyYonX*X[t]*Y[t];
lotkaVolterraY = fireEfficiencyXonY*X[t]*Y[t];
```

Osipov interactions:

```mathematica
osipovX = fireEfficiencyY*Y[t];
osipovY = fireEfficiencyX*X[t];
```

Equations:

```mathematica
lsEquations = {
     X'[t] == growthX - diseaseX*X[t] - fireFriendlyX*X[t] - lotkaVolterraX - osipovX, 
     Y'[t] == growthY - diseaseY*Y[t] - fireFriendlyY*Y[t] - lotkaVolterraY - osipovY 
   };
```

Make the model data structure:

```mathematica
model1 = <|
    "Rates" -> aRates, 
    "Stocks" -> aStocks, 
    "RateRules" -> aRateRules, 
    "InitialConditions" -> lsInitConds, 
    "Equations" -> lsEquations 
   |>;
```

Display the model in tabular format:

```mathematica
ModelGridTableForm[model1]
```

![0qpl0hdjgk2w9](./Diagrams/Generalized-Lanchester-combat-models/0qpl0hdjgk2w9.png)

------

## Direct model simulation

Here we follow the equations setup and data in the article [JE1].

**Remark:** The data in [JE1] is later revisited and new computations were made with almost the same results. See [RS1].

```mathematica
Clear[growthIwoJimaX];
growthIwoJimaX[t_] := Piecewise[{{54000, 0 <= t < 1}, {6000, 2 <= t < 3}, {13000, 5 <= t < 6}}, 0]
```

Here replace the constant term of the first equation in the generalized model with the growth function `growthIwoJimaX`.

```mathematica
model1a = model1 /. growthX -> growthIwoJimaX[t];
```

Here create the monadic pipeline, put the fire efficiency rates found in [JE1], simulate the for battles number of days, and plot the result:

```mathematica
sdObj =
  SDMonUnit[]⟹
   SDMonSetSingleSiteModel[model1a]⟹
   SDMonAssignRateRules[<|
     growthY -> 0,
     diseaseX -> 0, diseaseY -> 0,
     fireEfficiencyYonX -> 0, fireEfficiencyXonY -> 0,
     fireEfficiencyX -> 0.0106, fireEfficiencyY -> 0.0544
     |>]⟹
   SDMonAssignInitialConditions[<|X[0] -> 0, Y[0] -> 21000|>]⟹
   SDMonEcho[Style["Model's tabular form:", Bold, Purple, FontSize -> 18, FontFamily -> "Times"]]⟹
   SDMonEchoModelGridTableForm⟹
   SDMonSimulate[35]⟹
   SDMonEcho[Style["Plot simulated stocks evolution:", Bold, Purple, FontSize -> 18, FontFamily -> "Times"]]⟹
   SDMonPlotSolutions[];
```

![0qa3kyg6w6kyx](./Diagrams/Generalized-Lanchester-combat-models/0qa3kyg6w6kyx.png)

![0qdub5bcz7xzi](./Diagrams/Generalized-Lanchester-combat-models/0qdub5bcz7xzi.png)

![1o4m4i8y8qno1](./Diagrams/Generalized-Lanchester-combat-models/1o4m4i8y8qno1.png)

![0zqrokkfyfxo2](./Diagrams/Generalized-Lanchester-combat-models/0zqrokkfyfxo2.png)

-------

## Calibration

In this section we show that using optimization methods -- calibration -- we can obtain the same efficiency rates as the ones theoretically computed in [JE1] using the same data.

**Remark:** This should bring some confidence in using SDMon; and since the calibration process is easy to specify, it should encourage SDMon's use for other SD models.

Consider the following ***calibration targets*** that are "time series" each derived via linear interpolation from two data points -- the start- and end values of the number of soldiers for each army:

```mathematica
aTargets2 = N@
    <|
     X -> Map[Interpolation[{{0, Total@Map[growthIwoJimaX, Range[0, 36]]}, {35, 51300}}, InterpolationOrder -> 1], Range[0, 35]], 
     Y -> Interpolation[{{0, 21000}, {35, 0}}, InterpolationOrder -> 1] /@ Range[0, 35] 
    |>;
```

```mathematica
ListLinePlot[aTargets2, PlotTheme -> "Detailed"]
```

![04h6tiutk2v18](./Diagrams/Generalized-Lanchester-combat-models/04h6tiutk2v18.png)

Here is the first point of the stock $X$ (USA soldiers):

```mathematica
Total@Map[growthIwoJimaX, Range[0, 35]]

(*73000*)
```

Here is a model setup pipeline:

```mathematica
sdObj2 = 
   SDMonUnit[]⟹
    SDMonSetSingleSiteModel[model1a]⟹
    SDMonAssignRateRules[<|
      growthY -> 0, 
      diseaseX -> 0, diseaseY -> 0, 
      fireEfficiencyYonX -> 0, fireEfficiencyXonY -> 0 
     |>]⟹
    SDMonAssignInitialConditions[<|X[0] -> 0, Y[0] -> 21000|>]⟹
    SDMonEchoModelGridTableForm;
```

![0dn5n2wr30j8p](./Diagrams/Generalized-Lanchester-combat-models/0dn5n2wr30j8p.png)

Here is a calibration pipeline that specifies:

1. Target time series of the stocks

1. Parameters to calibrate

1. Parameter ranges, within which to search the calibration values

1. Distance function from the computed time series to the target time series

1. Parameters for [NMinimize](https://reference.wolfram.com/language/ref/NMinimize.html)

```mathematica
resCalibration = 
  sdObj2⟹
   SDMonCalibrate[
    "Target" -> KeyTake[aTargets2, {X, Y}], 
    "Parameters" -> <|fireEfficiencyX -> {0, 0.1}, fireEfficiencyY -> {0, 0.1}|>, 
    DistanceFunction -> EuclideanDistance, 
    Method -> {"NelderMead", "PostProcess" -> False}, 
    MaxIterations -> 1000]⟹
   SDMonTakeValue

(*{83614.5, {fireEfficiencyX -> 0.00990159, fireEfficiencyY -> 0.0399577}}*)
```

Here we plot the simulated army sizes using the calibrated values:

```mathematica
sdObj2⟹
  SDMonSimulate[35]⟹
  SDMonAssignRateRules[
   Association@resCalibration[[2]]]⟹
  SDMonEcho[
   Style["Plot simulated stocks evolution with calibrated values:", 
    Bold, Purple, FontSize -> 18, 
    FontFamily -> "Times"]]⟹
  SDMonPlotSolutions[];
```

![06slywccm35ey](./Diagrams/Generalized-Lanchester-combat-models/06slywccm35ey.png)

![1kbgrc3wyhku7](./Diagrams/Generalized-Lanchester-combat-models/1kbgrc3wyhku7.png)

Here we use a more "complicated" distance function that takes only the known points of the time series:

```mathematica
resCalibration2 = 
  sdObj2⟹
   SDMonCalibrate[
    "Target" -> KeyTake[aTargets2, {X, Y}], 
    "Parameters" -> <|fireEfficiencyX -> {0, 0.1}, fireEfficiencyY -> {0, 0.1}|>, 
    DistanceFunction -> (EuclideanDistance[#1[[{1, -1}]], #2[[{1, -1}]]] &), 
    Method -> {"NelderMead", "PostProcess" -> False}, MaxIterations -> 1000]⟹
   SDMonTakeValue

(*{73000., {fireEfficiencyX -> 0.0106351, fireEfficiencyY -> 0.0547895}}*)
```

**Remark:** Note that with that new calibration distance function we get almost the same results as the theoretically computed efficiency rates in [JE1]:

```
<|fireEfficiencyX -> 0.0106, fireEfficiencyY -> 0.0544|>
```

**Remark:** Proper calibration time series for the USA soldiers stock $X$ can be obtained from the web page ["Iwo Jima, a look back"](https://www.recordsofwar.com/iwojimahistory/). (The corresponding data ingestion notebook is going to be published soon.)

------

## Future plans

Here are a few directions to extend this work into:

- Inclusion of different types of forces

- Simulation of "will to fight"

    - Easy with NDSolve and, hence, with SDMon.

- Inclusion of weapon and ammunition production stocks and related supply rates

    - For example, as in [AA2].

- Modeling the war impact on countries' economics and populations

- Modeling the role of propaganda

- Make interactive interfaces with knobs for the parameters

    - With selectors of scenarios based on known battles.

## Setup

```mathematica
PacletInstall["AntonAntonov/EpidemiologicalModeling"];
PacletInstall["AntonAntonov/MonadicSystemDynamics"];
```

```mathematica
Needs["AntonAntonov`EpidemiologicalModeling`"]
Needs["AntonAntonov`MonadicSystemDynamics`"]
```

-------

## References

### Articles

[AA1] Anton Antonov, ["A monad for Epidemiologic Compartmental Modeling Workflows"](https://mathematicaforprediction.wordpress.com/2021/01/02/epidemiology-compartmental-modeling-monad/), (2021), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com/2021/01/02/epidemiology-compartmental-modeling-monad/).

[AS1] Andrei Shatyrko, Bedrik Puzha, Veronika Novotná, "Comparative Analysis and New Field of Application Lanchester’s Combat Models",  (2018), Post-conference proceedings of selected papers extended version Conference MITAV-2018, Brno, Czech Republic, 2018. P.118-133. ISBN 978-80-7582-065-5.

[JE1] J.H. Engel, "A verification of Lanchester's law", (1953), Journal of the Operations Research Society of America, Vol. 2, No. 2. (May, 1954), pp. 163-171. ([JSTOR link](https://www.jstor.org/stable/166602).)

[DR1] J. David Rogers, ["Iwo Jima: The costliest battle in  American history"](https://web.mst.edu/~rogersda/american&military_history/BATTLE-OF-IWO-JIMA.pdf), Missouri S&T.
([jdavidrogers.net](https://www.jdavidrogers.net/).)

[NM1] Nikolai V. Mityukov. Determining the casualties of wars through Lanchester models, (2009), [Historical Psychology and Sociology of History](https://www.socionauki.ru/journal/ipisi/).

[NM1ru] Н. В. МИТЮКОВ, [ОПРЕДЕЛЕНИЕ ЖЕРТВ ВОЙН ЧЕРЕЗ ЛАНЧЕСТЕРСКИЕ МОДЕЛИ](https://www.socionauki.ru/journal/articles/130365/?sphrase_id=562105), [Историческая психология и социология истории](https://www.socionauki.ru/journal/ipisi/) 2/2009 122–140.

[RS1] Robert W. Samz, ⎡Some Comments on Engel's "A Verification of Lanchester's Law"⎦, (1972), Operations Research, Vol. 20, No. 1 (Jan. - Feb., 1972), pp. 49-52 (4 pages) ([JSTOR link](https://www.jstor.org/stable/169337).)

[Wk1] [Lanchester's laws, Wikipedia](https://en.wikipedia.org/wiki/Lanchester%27s_laws).

[Wk2] [Battle of Iwo Jima, Wikipedia](https://en.wikipedia.org/wiki/Battle_of_Iwo_Jima).

### Books

[VA1] Vladimir I. Arnold, Rigid and soft mathematical models, 2nd ed. (2008), Moscow Center of Continuous Mathematical Education.
In Russian:  Владимир И. Арнольд, "«Жесткие» и «мягкие» математические модели", (2008), М.: МЦНМО, 2014, 32 с. ISBN 978-5-94057-427-9.

### Packages, paclets

[AAp1] Anton Antonov, [EpidemiologicalModeling](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/EpidemiologicalModeling/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

[AAp2] Anton Antonov, [MonadicSystemDynamics](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/MonadicSystemDynamics/), WL paclet, (2023), [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/).

### Repositories

[AAr1] Anton Antonov, [System Modeling](https://github.com/antononcube/SystemModeling), (2020-2023), [GitHub/antononcube](https://github.com/antononcube).