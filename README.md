# Reltrad in R and Stata
This code recodes the General Social Survey data on religious tradition and codes into the Reltrad categories to match Steensland et al. This uses a mix of variables in the GSS, denom, other, and race, among others.

You need to download the GSS file in Stata format from NORC's website and edit the file ReltradGSS.R to reflect where the file is stored. The code assumes that the data is in a folder called "Data".

You must remove the black oversample from the GSS and also calculate final proportions using weights. This is all done in the GGplotReltrad.R code that produces the graph of Reltrad from 1972 to 2018. Look at the output folder for this graph.

email me with questions: [david.eagle@duke.edu](mailto:david.eagle@duke.edu)
