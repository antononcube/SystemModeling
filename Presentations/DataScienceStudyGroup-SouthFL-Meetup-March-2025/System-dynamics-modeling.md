
### System Dynamics Modeling

Generated using Grok with the prompt:

> Give the System Dynamics (SD) model for that outline:
> - Clearly indicate what the stocks and what the rates
> - For each rate give a plausible value with the corresponding measure units
> - Write the SD equations in LaTeX

<hr style="border: 1px solid dimgray; width:60%"></hr>

## SD model structure

### Stocks
1. **Fire Size (FS)**: The area of the fire (in square meters, m²), representing the extent of the burning region.
2. **Water Available (WA)**: The volume of water available for firefighting (in cubic meters, m³).
3. **Water Used (WU)**: The cumulative volume of water applied to the fire (in cubic meters, m³).
4. **Suppressed Fire Area (SFA)**: The area of the fire that has been extinguished (in square meters, m²).

### Flows (Rates)
1. **Fire Growth Rate (FGR)**: Rate at which the fire spreads (m²/min).
2. **Fire Suppression Rate (FSR)**: Rate at which the fire is extinguished (m²/min).
3. **Water Supply Rate (WSR)**: Rate at which water is replenished (m³/min).
4. **Water Application Rate (WAR)**: Rate at which water is applied to the fire (m³/min).

### Auxiliary Variables
- **Fire Intensity (FI)**: Water needed per unit area (m³/m²/min), influencing suppression.
- **Resource Capacity (RC)**: Combined capacity of firefighting resources (e.g., trucks, hydrants) in m³/min.

---

## Plausible Values for Rates
These are sample values based on typical urban firefighting scenarios:
1. **Fire Growth Rate (FGR)**: 10 m²/min (typical for a residential fire with moderate fuel load and ventilation).
2. **Fire Suppression Rate (FSR)**: 15 m²/min (assuming adequate water and personnel).
3. **Water Supply Rate (WSR)**: 0.05 m³/min (e.g., a single hydrant at 3,000 LPM = 3 m³/min, scaled for simplicity).
4. **Water Application Rate (WAR)**: 0.033 m³/min (2,000 LPM = 2 m³/min, a conservative estimate for two firetrucks).

---

## SD Equations in LaTeX

### Stock Equations
Stocks change over time based on inflows and outflows. Using differential equations:

1. **Fire Size (FS)**:
   $$
   \frac{dFS}{dt} = FGR - FSR
   $$
- Initial condition: $ FS(0) = 50 $ m² (small initial fire).

2. **Water Available (WA)**:
   $$
   \frac{dWA}{dt} = WSR - WAR
   $$
- Initial condition: $ WA(0) = 10 $ m³ (e.g., onboard water from two firetrucks).

3. **Water Used (WU)**:
   $$
   \frac{dWU}{dt} = WAR
   $$
- Initial condition: $ WU(0) = 0 $ m³.

4. **Suppressed Fire Area (SFA)**:
   $$
   \frac{dSFA}{dt} = FSR
   $$
- Initial condition: $ SFA(0) = 0 $ m².

### Flow Equations
Rates depend on stocks, auxiliary variables, and external factors:

1. **Fire Growth Rate (FGR)**:
   $$
   FGR = k_g \cdot FS \cdot (1 - \frac{FS}{K})
   $$
- $ k_g = 0.2 $ min⁻¹ (growth constant, exponential early growth).
- $ K = 500 $ m² (carrying capacity, max fire size due to fuel or space limits).
- Units: m²/min.
- Notes: Logistic growth assumes fire slows as it consumes available fuel.

2. **Fire Suppression Rate (FSR)**:
   $$
   FSR = \min\left(FS, \frac{WAR}{FI}\right)
   $$
- $ FI = 0.002 $ m³/m²/min (water intensity, e.g., 2 L/m²/min = 0.002 m³/m²/min).
- Units: m²/min.
- Notes: Suppression is limited by either fire size or water application.

3. **Water Supply Rate (WSR)**:
   $$
   WSR = 0.05
   $$
- Units: m³/min.
- Notes: Constant supply from hydrants (e.g., 3 m³/min scaled down for simplicity).

4. **Water Application Rate (WAR)**:
   $$
   WAR = \min\left(WA, RC\right)
   $$
- $ RC = 0.033 $ m³/min (resource capacity, e.g., 2,000 LPM = 2 m³/min scaled).
- Units: m³/min.
- Notes: Limited by water availability or firefighting equipment.

---

## Model Dynamics
- **Fire Growth**: $ FS $ increases via $ FGR $ until suppression ($ FSR $) overcomes it or fuel is exhausted.
- **Water Management**: $ WA $ decreases as water is applied ($ WAR $) but is replenished by $ WSR $. $ WU $ tracks total usage.
- **Suppression**: $ SFA $ grows as $ FSR $ extinguishes $ FS $, ideally reducing $ FS $ to zero.

---

## Example Simulation (Conceptual)
- **t = 0**: $ FS = 50 $ m², $ WA = 10 $ m³, $ WU = 0 $ m³, $ SFA = 0 $ m².
- **t = 5 min**:
    - $ FGR \approx 10 $ m²/min, $ FS $ grows to ~100 m² without suppression.
    - With $ WAR = 0.033 $ m³/min and $ FI = 0.002 $, $ FSR = 16.5 $ m²/min.
    - $ FS $ reduces to ~25 m², $ WA $ drops to ~9.8 m³, $ WU = 0.165 $ m³, $ SFA = 75 $ m².
- **t = 10 min**: Fire potentially extinguished if $ FSR > FGR $ persists.

---

## Notes
- **Simplifications**: Constant $ WSR $, no wind or structural collapse effects.
- **Scalability**: Adjust $ RC $ and $ WSR $ for larger fires or more resources.
- **Validation**: Calibrate $ k_g $ and $ FI $ with real fire data.

This SD model provides a dynamic framework for urban fire management. Let me know if you’d like to refine it further or simulate specific scenarios!