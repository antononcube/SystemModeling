# Coronavirus propagation modeling

## In brief

In this (tutorial) presentation we are going to:
1. Discuss the modeling and evolution of different scenarios of the coronavirus propagation and related decease
2. Overview classic compartmental models in epidemiology
4. Discuss the theory behind the System dynamics approach using simple examples
3. Discus philosophical, scientific, and mathematical justifications
5. Describe System Dynamics' approaches of modeling the propagation of
[SARS-CoV-2](https://en.wikipedia.org/wiki/Severe_acute_respiratory_syndrome_coronavirus_2) 
and the economic impact of 
[COVID-19](https://en.wikipedia.org/wiki/Coronavirus_disease_2019). 

Our focus is on the modeling strategy and techniques and how they can be implemented using R.

Most of the examples used to illustrate the theoretical concepts and practical aspects will have 
graphics visualizations. The more important examples will have interactive interfaces. 

Most of the models are not coronavirus specific and the presentation can be seen as an introduction to System dynamics.

## Part 1 -- Coronavirus outbreak overview (7-10 min)

In the first part we are going to describe the "2019-2020 coronavirus outbreak problem" and
establish related terminology.

## Part 2 -- Compartmental models and System dynamics models (30 min)

In the second part we are going to
1. Discuss and outline the classic compartmental models in epidemiology using several examples
2. Introduce, clarify, and demonstrate the basics of SD's theory and methodology 
using progressively more complicated examples.

First we will see the mathematical models and related R code. 
After that we will show how different interactive interfaces (made with Shiny) can be utilized to better understand
the results.

## Part 3 -- Coronavirus Scenario modeling and simulations (20 min)

In the last part we are going to show how to model various propagation, policy, and economic aspects related to the 
coronavirus 2019-20 outbreak. 

We focus on the different strategies to model the outbreak dynamics and how the different 
states can be interpreted from economics point of view.

We show how to progressively built complicated models starting from (relatively) simple ones. 
We also discuss the interpretation of the simulation outcomes.


## References

### Coronavirus 

\[Wk1\] Wikipedia entry, 
[Severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2)](https://en.wikipedia.org/wiki/Severe_acute_respiratory_syndrome_coronavirus_2).

\[Wk2\] Wikipedia entry,
[Coronavirus disease 2019](https://en.wikipedia.org/wiki/Coronavirus_disease_2019).

### Modeling

\[Wk3\] Wikipedia entry,
[Compartmental models in epidemiology](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology).

\[Wk4\] Wikipedia entry,
[System dynamics](https://en.wikipedia.org/wiki/System_dynamics).

\[JD1\] Jim Duggan, 
[System Dynamics Modeling with R](https://www.springer.com/gp/book/9783319340418), 
2016, Springer.

### R code/software

\[KS1\] Karline Soetaert, Thomas Petzoldt, R. Woodrow Setzer,
["deSolve: Solvers for Initial Value Problems of Differential Equations ('ODE', 'DAE', 'DDE')"](https://cran.r-project.org/web/packages/deSolve/index.html),
[CRAN](https://cran.r-project.org). 

\[JD2\] Jim Duggan, 
["SDMR"](https://github.com/JimDuggan/SDMR), 
2016, 
[GitHub](https://github.com/JimDuggan).   
("Resources for text book "System Dynamics Modeling with R".)
 
 