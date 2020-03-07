# Coronavirus propagation modeling considerations

Anton Antonov  
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)  
[SystemModeling at GitHub](https://github.com/antononcube/SystemModeling)  
March 2020

## Introduction

In this document (notebook) we want to list the minimum of definitions, finds, and data sources for doing interesting computational models of Coronavirus strains propagation and related decease management. 

We aim at making models for the propagation of [Severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2)](https://en.wikipedia.org/wiki/Severe_acute_respiratory_syndrome_coronavirus_2), [Wk1], and the spread, control, and management of the related [Coronavirus Disease 2019 (COVID-19)](https://en.wikipedia.org/wiki/Coronavirus_disease_2019), [Wk2]. We especially interested in providing answers to different “what-if” scenarios for making policy decisions that tackle COVID-19. 

## Definitions

TBD...

## Summary of the SARS-CoV-19 and COVID-19

### What CDC says?

See the dedicated Centers for Disease Control and Prevention (CDC) site [Coronavirus Disease 2019 (COVID-19)](https://www.cdc.gov/coronavirus/2019-ncov/index.html). 

```mathematica
WebImage["https://www.cdc.gov/coronavirus/2019-ncov/index.html"]
```

![1quyxj1d34uf5](./Diagrams/Coronavirus-propagation-modeling-considerations/1quyxj1d34uf5.png)

### Form Slashdot.org

[The following summary points](https://science.slashdot.org/comments.pl?sid=15840894&cid=59762854) are taken from [Slashdot.org](https://slashdot.org)’s discussion ["Coronavirus Outbreak Has ‘Pandemic Potential’ But It’s Not There Yet, WHO Says"](https://science.slashdot.org/story/20/02/24/2050254/coronavirus-outbreak-has-pandemic-potential-but-its-not-there-yet-who-says): 

- The virulence (R0) of SARS-CoV-2 is estimated between 1.4-6.49, with a mean estimate of 3.28 . This mean estimate is much higher than the seasonal flu, which has an R0 of 1.3 . What this means is that SARS-CoV-2 spreads significantly faster than the seasonal flu.

- The Case Fatality Rate (CFR) of SARS-CoV-2 is at least 2-3% . This is 20-30 times higher than the CFR of the season flu, which is below 0.1% .  
SARS-CoV-2 can be transmitted without the infected showing any symptoms . This makes it much more difficult to control.

- Roughly 20% of SARS-CoV-2 infections result in serious symptoms that require medical intervention . This is more than 10 times the hospitalization rate of the seasonal flu.

- Symptoms from SARS-CoV-2 can persist over a month compared to the seasonal flu where symptoms typically tend to clear after 5 days.

- There is no vaccine for SARS-CoV-2 whereas people regularly get annual flu shots.

- There is no herd immunity for SARS-CoV-2 which means that it can theoretically infect the entire population. See, for example, a Korean psychiatric department where the virus infected 99/102 people.

- Now consider the multiplicative effect that all of these attributes have for the virus. Compared to the seasonal flu, SARS-CoV-2 (1) spreads faster; (2) kills far more; (3) is harder to control; (4) requires use of far more medical resources; (5) for far longer a period of time; (6) has no effective treatment; and (7) can infect entire populations.

- These factors mean that SARS-CoV-2, if left unchecked, is far more likely to overwhelm a country's medical infrastructure. Additionally, when medical infrastructure is overwhelmed, the CFR will skyrocket because we know that 20% of cases require medical intervention.

- It doesn't take a genius to piece it all together. This virus is potentially devastating if containment measures fail. Far worse than the seasonal flu.

## Breakdown

TBD...

Epidemic model comprehensiveness 
Age compartments
Geo compartments

Economic model comprehensiveness 
New beds
New drugs or medical supplies.
Delivery
Enforcement (of policy)


## Data analysis

Various data analysis is done by different parties over different aspects of the SARC-CoV-2 and COVID-19. 

Here we just mention a few references that can be used to build sufficient impressions for building models.

TBD ...

## Modeling approach

### First simple model

Generally speaking, the [System Dynamics (SD)](https://en.wikipedia.org/wiki/System_dynamics) methodology is used.

The first model uses a lot of simplifying assumptions and deals with mesoscale SARS-CoV-19 propagation without comprehensive breakdown of age groups.

### Model extension plan

TBD ..

### Calibration

Calibration of the models should be done -- or at least attempted -- for a particular geographic region or the world. The calibration efforts would indicate to what degree the models correspond to reality, what levels of model comprehensiveness are required or sufficient.

## References

### Articles

[Wk1] Wikipedia entry, [Severe acute respiratory syndrome coronavirus 2](https://en.wikipedia.org/wiki/Severe_acute_respiratory_syndrome_coronavirus_2).

[Wk2] Wikipedia entry, [Coronavirus disease 2019](https://en.wikipedia.org/wiki/Coronavirus_disease_2019).

### Data

TBD...

### Repositories

[AAr1] Anton Antonov, [Coronavirus propagation dynamics project](https://github.com/antononcube/SystemModeling/tree/master/Projects/Coronavirus-propagation-dynamics), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).
