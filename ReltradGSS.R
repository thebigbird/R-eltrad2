#Using the reltrad function:
#For easier recoding
library(car)
library(gssr)
library("tidyverse")
library(descr) #Get the CrossTable Function! Weighted! crosstab
data(gss_all)
gss=gss_all;rm(gss_all)

#Start
#Get rid of the Black oversample
source("")
gss = gss %>% 
  filter(sample!=4,sample!=5,sample!=7) %>% 
  filter(year<2021)

gss = reltrad(gss)

gss %>% summarize(mean(reltrad))