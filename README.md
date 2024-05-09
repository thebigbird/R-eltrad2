# R-eltrad2 in R
The most commonly used taxonomy for categorizing U.S. survey respondents into their respective religious traditions is affectionately known as _reltrad_. Steensland, B., L. D. Robinson, W. B. Wilcox, J. Z. Park, M. D. Regnerus, and R. D. Woodberry. 2000. "The Measure of American Religion: Toward Improving the State of the Art." _Social Forces_ 79 (1): 291–318. [https://doi.org/10/db9hrh](https://doi.org/10/db9hrh).

Recently, Josh Gaghan and I published an update to _reltrad_, called **_Reltrad2_**. **_Reltrad2_** improves how nondenominational and inter-denominational people are classified. In this article, we detail the reasons for our choices: Gaghan, Joshua & David Eagle. 2024. "RELTRAD2: Refining the State of the Art of Religious Classification by Reconsidering the Categorization of Nondenominational Respondents."
_Journal for the Scientific Study of Religion_. [https://doi.org/10.1111/jssr.12916](https://doi.org/10.1111/jssr.12916).

## Code to Generate Religious Tradition (**_reltrad2_**)
A Stata file was created long ago to make the _reltrad_ variable. I translated it directly to R. I've since modified the code to produce **_reltrad2_**. To use this code, you need to download the GSS file in Stata format from NORC's website and edit the file ReltradGSS.R to reflect where the file is stored. The code assumes that the data is in a folder called "Data." Remember to use **_reltrad2_** in the GSS; you must remove the black oversample from the GSS and calculate the final proportions using weights. This repository contains code (GGplotReltrad.R) that produces sample output.

Other datasets will need a customized approach.

email me with questions: [david.eagle@duke.edu](mailto:david.eagle@duke.edu)
