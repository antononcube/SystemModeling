# Analogy between SEIR and Urban fire propagation

Generated using GPT-4o with the prompt:

> Give details of the following analogy between the compartmental epidemiological SEIR model and Urban file model over a neighborhood of houses:
>
> - Each house consist of millions of fuel units (denote with HFU)
>
> - For a given house different fractions of HFUs are:
>    - In their normal state
>    - Exposed to heat radiation and/or flying embers
>    - In flames
>    - Burned out completely or embers
>
> The corresponding to the HFUS fractions SEIR populations are:
> - Susceptible
> - Exposed
> - Infected
> - Recovered / died
>
> If convenient use formulas and tables to explain the analogy.

<hr style="border: 1px solid dimgray; width:60%"></hr>

The analogy between the compartmental epidemiological SEIR model and an urban fire model over a neighborhood of houses can be understood by comparing the stages of disease progression in a population to the stages of fire progression in a house. Here's how the analogy can be detailed:

### SEIR Model in Epidemiology:
- **Susceptible (S):** Individuals who are not yet infected but are at risk of infection.
- **Exposed (E):** Individuals who have been exposed to the infection but are not yet infectious.
- **Infected (I):** Individuals who are currently infectious and can spread the disease.
- **Recovered/Deceased (R):** Individuals who have recovered from the disease or have died.

### Urban Fire Model:
- **House Fuel Units (HFU):** Analogous to individual units within a house that can be in different states during a fire.

#### Corresponding States:
1. **Susceptible (S) → Normal State HFUs:**
    - These are the fuel units that have not yet been affected by the fire. They are analogous to susceptible individuals in an epidemiological model.

2. **Exposed (E) → Exposed HFUs:**
    - These fuel units are exposed to heat radiation and/or flying embers but have not yet ignited. They are analogous to exposed individuals who have come into contact with the disease but are not yet infectious.

3. **Infected (I) → In Flames HFUs:**
    - These are the fuel units that are actively burning. They represent the infectious stage in the epidemiological model where individuals can spread the infection (or fire, in this case) to other units.

4. **Recovered/Deceased (R) → Burned Out/Embers HFUs:**
    - These fuel units have been completely consumed by the fire or reduced to embers. They are analogous to individuals who have either recovered from the disease or have died, and thus no longer contribute to the spread of the infection.

### Mathematical Representation:
In the SEIR model, the transition between compartments can be described by differential equations. Similarly, the transition between different states of HFUs can be modeled mathematically:

- **Susceptible to Exposed:**

  $$
  \frac{dS}{dt} = -\beta S I
  $$
  In the fire model, this can be represented as the rate at which normal HFUs become exposed due to heat or embers.

- **Exposed to Infected:**

  $$
  \frac{dE}{dt} = \beta S I - \sigma E
  $$
  This represents the rate at which exposed HFUs catch fire.

- **Infected to Recovered/Deceased:**

  $$
  \frac{dI}{dt} = \sigma E - \gamma I
  $$
  This describes the rate at which burning HFUs are consumed and transition to burned out or ember state.

- **Recovered/Deceased:**

  $$
  \frac{dR}{dt} = \gamma I
  $$
  This represents the accumulation of HFUs that are completely burned out or reduced to embers.

### Typical Values and Considerations:
- **Transmission Rate ($\beta$):** In fire modeling, this could relate to the flammability of materials and proximity of houses.
- **Incubation Rate ($\sigma$):** In fire, this could relate to the time it takes for exposed HFUs to ignite.
- **Recovery Rate ($\gamma$):** In fire, this could relate to the rate at which burning materials are consumed.

This analogy helps in understanding how fire spreads through a neighborhood by drawing parallels to disease transmission in a population, allowing for the use of similar mathematical and simulation techniques to predict and manage urban fires.