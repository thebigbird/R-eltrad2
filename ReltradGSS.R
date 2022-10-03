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
source("https://raw.githubusercontent.com/thebigbird/R_Stata_Reltrad/master/reltradFn.R")
gss = gss %>% 
  filter(sample!=4,sample!=5,sample!=7) %>% 
  filter(year<2021)

#Create a reltrad variable
gss = gss %>% mutate(reltrad = reltrad(gss))

#Here's a tibble of frequencies by year
gss %>% group_by(year,reltrad) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n/sum(n))
  
