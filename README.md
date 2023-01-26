# Experience of Groove Questionnaire (EGQ)
R package to include the Experience of Groove Questionnaire (Senn et al., 2020) as various psychTestR (Harrison, 2020) functions

## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install the EGQ:

`devtools::install_github('KilianSander/groovescale')`

## Implemented variants of the EGQ
The Experience of Groove Questionnaire is implemented as a single page (`EGQ`) and with one page per item (`GRV`).
Both can be used within a battery of tests and questionnaires (`EGQ()` or `GRV()`), i.e. a psychTestR timeline,
or in their standalone versions (`EGQ_standalone()` or `GRV_standalone()`).

## References
Harrison, P. M. C. (2020). psychTestR: An R package for designing and conducting behavioural psychological experiments. *Journal of Open Source Software*. https://doi.org/10.21105/joss.02088

Senn, O., Bechtold, T., Rose, D., Câmara, G. S., Düvel, N., Jerjen, R., Kilchenmann, L., Hoesl, F., Baldassarre, A., & Alessandri, E. (2020). Experience of groove questionnaire: Instrument development and initial validation. *Music Perception, 38*(1), 46-65. https://doi.org/10.1525/mp.2020.38.1.46
