# Coronavirus propagation dynamics

This project has models for different aspects of Coronavirus propagation dynamics.
More specifically the focus is on the propagation of 
[SARS-CoV-2](https://en.wikipedia.org/wiki/Severe_acute_respiratory_syndrome_coronavirus_2)
and the economic impact of 
[COVID-19](https://en.wikipedia.org/wiki/Coronavirus_disease_2019). 

## The model

The following mind-map illustrates the scope of the considered model:

![Coronavirus-propagation-modeling-mind-map](./Diagrams/Coronavirus-propagation-modeling-mind-map.png)

## Work plan

The work plan and its completion state can be found 
[here](./org/Coronavirus-propagation-work-plan.org).

The following graph shows the development plan:
 
 ![Coronavirus-propagation-modeling-development-plan-graph](./Diagrams/Coronavirus-propagation-modeling-development-plan-graph.png)
 
*(The check marks indicate complication the triangles indicate current focus.)*
 
## Data analysis

In order to build or confirm certain modeling conjectures and assumptions certain data analysis is required.

(TBD...)

## Simple epidemiology models

A small, lightweight software framework for retrieval of the basic 
[compartmental epidemiology models](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology).

The framework consists of two packages:

- [EpidemiologyModels.m](./WL/EpidemiologyModels.m), and

- [EpidemiologyModelModifications.m](./WL/EpidemiologyModelModifications.m).

See the document 
["Basic experiments workflow for simple epidemiological models"](./Documents/Basic-experiments-workflow-for-simple-epidemiological-models.md)
for the envisioned general workflow, interactive interfaces, and ad-hoc calibration.   


## The SEI2R-Econ model

Generally speaking, the 
[System dynamics](https://en.wikipedia.org/wiki/System_dynamics) 
methodology is used. 

The first model uses a lot of simplifying assumptions and deals only with:

- four types of populations (meaning no age is included),

- available hospital beds,

- medical suppliers, and

- delivery of medical supplies.

Nevertheless, that simple model provides certain insights and it is a good starting point for multiple types of
model extensions. 

Here is a component interaction diagram:

![Coronavirus-propagation-simple-dynamics](./Diagrams/Coronavirus-propagation-simple-dynamics.jpeg)

Here is "a teaser" of how the simple model interactive interface looks like:

![Coronavirus-propagation-interactive-modeling-teaser](./Diagrams/Coronavirus-propagation-interactive-modeling-teaser-2.png)

## Model extensions plan

TBD...
 

## Calibration

TBD...

## References

\[1\] https://www.cdc.gov/coronavirus/2019-ncov .