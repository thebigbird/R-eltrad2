#For easier recoding
#Get rid of the black oversamples - these throw off the proportions
#This takes the 72-18 GSS and recodes it to create reltrad. 
#Small changes made Nov. 8, 2021.
#PLEASE UPDATE YOUR CODE

#CONTAINS LIFEWAY CODE CORRECTION
#http://lifewayresearch.com/wp-content/uploads/2016/02/Stata_coding_reltrad_2_19_2016.pdf

#Created by David Eagle www.davideagle.org
#"code samples/GGplotReltrad7218.R" makes a picture of religious tradition over time

#For easier recoding
library(car)
library(gssr)
library("tidyverse")
library(descr) #Get the CrossTable Function! Weighted! crosstab
data(gss_all)
gss=gss_all;rm(gss_all)

#Start
#Get rid of the Black oversample
gss = gss %>% 
  filter(sample!=4,sample!=5,sample!=7) %>% 
  filter(year<2021)

#recode into 5 major categories of religious affiliation
# 1) Protestant [Ask DENOM]	1371	47.8
# 2) Catholic
# 3) Jewish	
# 4) None	
# 5) Other (specify)
# 6) Buddhism
# 7) Hinduism
# 8) Other Eastern religion
# 9) Muslim/Islam
# 10) Orthodox Christian
# 11) Christian
# 12) Native American
# 13) Inter-/non-denominational
# 98) Don't know
# 99) No answer

gss$reltrad=NA
gss$xaffil = car::recode(gss$relig, "1=1;2=4;3=5;4=9;5:10=6;11=1;12=6;13=1")

gss$xaffil = as.factor(gss$xaffil)
levels(gss$xaffil) = c("prot", "cath", "jew", "other", "nonaf")

# The following code breaks down religious groups by evangelicals, black
# Protestants, mainline, liberal and conservative nontraditional,
# and Protestant nondenomination/no denomination.

#####Black Protestants
#####
#Create a racial indicator using the GSS "race" variable in 1972-2018
gss$black = ifelse(gss$race == 2, 1, 0)
gss$white = ifelse(gss$race == 1|gss$race == 3, 1, 0)

#Take the "other" Protestant denominations and pull out the 
#historical Black denominations, e.g. COGIC
gss$xbp = gss$other

gss$xbp = ifelse(gss$xbp %in% c(7, 14, 15, 21, 37, 38, 56, 78, 79, 85, 86, 
                                87, 88, 98, 103, 104, 128, 133), 1, 0)

#National baptists and AME, AMEZ
gss$xbp = ifelse(gss$denom %in% c(12, 13, 20, 21), 1, gss$xbp)

#Blacks in certain denoms get recoded as Black Protestant
#Other baptist, amer. baptist, south. bap, other Methodists
# Create indicator for black race in denom and other
gss$bldenom = 0
gss$bldenom = gss$denom * gss$black
gss$blother = gss$other==93 * gss$black
#Call these black denominations

gss$xbp = ifelse(gss$bldenom %in%
                   c(23, 28, 18, 15, 10, 11, 14), 1, gss$xbp)
#Black missionary baptists
gss$xbp[gss$blother == 1] = 1

#Evangelical Protestants#
#Recode the evangelicals in the other variable
gss$xev=gss$other
evother=c(2, 3, 5, 6, 9, 10, 12, 13, 16, 18, 20, 22, 24, 26, 27, 28, 31, 32,
          34, 35, 36, 39, 41, 42, 43, 45, 47,51, 52, 53, 55, 57, 63, 65, 66, 
          67, 68, 69, 76, 77, 83, 84, 90, 91, 92, 94, 97, 100, 101, 102, 106,
          107, 108, 109, 110, 111, 112, 115, 116, 117, 118, 120, 121, 122, 124, 
          125, 127, 129, 131, 132, 134, 135, 138, 139, 140, 146)
gss$xev=ifelse(gss$xev %in% evother,1,0)
#Cons Lutherans, cons presbyterians
gss$xev=ifelse(gss$denom %in% c(32,33,34,42), 1, gss$xev)
#White baptists, white other methodists
gss$whdenom = gss$denom * gss$white

gss$xev = ifelse(gss$whdenom %in%  c(10,18,15,23,14),1,gss$xev)
#Missionary baptist
gss$wother = gss$other * gss$white
gss$xev[gss$wother==93] = 1
gss$xev[gss$xbp == 1] = 0
gss=as.data.frame(gss)

# Mainline Protestants
gss$xml = NA
gss$xml = gss$other
mpother=c(1,8,19,23,25,40, 44, 46, 48, 49, 50, 54, 70, 71, 72, 73, 81, 
          89, 96, 99, 105, 119, 148)
gss$xml=ifelse(gss$xml %in% mpother,1,0) 
#The denom category
gss$xml = ifelse(gss$denom %in% 
                   c(30, 50, 35, 31, 38, 40, 48, 43, 22, 41),1,gss$xml)

#Mainline baptist denom and methodists - if the R is white, they get coded mainline
gss$xml = ifelse(gss$whdenom %in% c(11, 28),1,gss$xml)

#Catholics
gss$xcath = gss$other 
#Polish National Church and Catholic
gss$xcath = ifelse(gss$other %in% c(123, 28),1,0)
#People who say that they are other get coded zero
gss$xcath=ifelse(gss$xaffil=="cath", 1, gss$xcath) 

#Jews
gss$xjew=0 
gss$xjew=ifelse(gss$xaffil=="jew",1,0)

#Adherents of other religions.
gss$xother = gss$other

gss$xother = ifelse(gss$xother %in% 
                      c(11, 17, 29, 30, 33, 58, 59, 60, 61, 62, 64, 74, 75, 80, 
                        82, 95, 113, 114, 130, 136, 141, 145),1,0)
#Adds others from main religious recoding
gss$xother=ifelse((gss$xaffil=="other" & gss$xev==0),1,gss$xother)

#Unaffiliateds/Nonaffiliateds
gss$xnonaff=0
gss$xnonaff[gss$xaffil=="nonaf"]=1

# #The recodes non-denoms based on their attendance 
# #Non active Don't Know Protestants coded to nonaffil
gss$xprotdk = ifelse(gss$denom == 70 ,1,0) #Assuming Christian here
# #Protestants who attend monthly or less get turned to mainliners
gss$xprotdk[gss$xprotdk == 1 & gss$attend < 4] = 0
# #Active Don't Know Protestants coded to evangelicals who attend 
gss$xev[gss$xprotdk == 1] = 1
#

#############################################
#*This takes people who responded that they were Christian in the relig variable but didn't get asked the
#followup and puts them into reltrad*
#http://ryanburge.net/wp-content/uploads/2015/12/reltradarticle-1.pdf
gss$xtn = gss$relig
gss$denom2 = gss$denom

#70 = No denomination or non-denominations
gss$denom2 = car::recode(gss$denom2, "70=1; else=0")
gss$xtn = car::recode(gss$xtn, "11=1; else=0")
gss$xtn[gss$denom2 == 1] = 2
gss$xtn = car::recode(gss$xtn, "1=1; 2=0")

#Only weekly or +weekly attenders - this is a questionable assumption
#Probably could be challenged.
gss$xtn[gss$attend < 4|is.na(gss$attend)] <- 0
gss$xev[gss$xtn ==1] <- 1
#This takes people who responded that they were Interdenominational in the relig variable but didn't
#get asked the followup and puts them into reltrad
gss$inter <- gss$relig

gss$inter <- car::recode(gss$inter, "13=1; else=0")
gss$inter[gss$attend < 4|is.na(gss$attend)] <- 0
gss$xev[gss$inter ==1] <- 1
#############################################

gss$reltrad = factor(NA, levels=c("Conservative Protestant",
                                  "Mainline Protestant",
                                  "Black Protestant",
                                  "Roman Catholic",
                                  "Jewish",
                                  "Other",
                                  "None"))

gss$reltrad[gss$xev==1]="Conservative Protestant"
gss$reltrad[gss$xml==1]="Mainline Protestant"
gss$reltrad[gss$xbp==1]="Black Protestant"
gss$reltrad[gss$xcath==1]="Roman Catholic"
gss$reltrad[gss$xjew==1]="Jewish"
gss$reltrad[gss$xother==1]="Other"
gss$reltrad[gss$xnonaff==1]="None"
gss$year = as.factor(gss$year)
gss = as.data.frame(gss)
#This codes the NA's as explicit
#mutate(reltrad = forcats::fct_explicit_na(reltrad, na_level = "(Missing)"))
#
#saveRDS(gss,file="gss7218_reltrad.csv")
#End of my poorly written R code! Sorry - I'll clean it up some day!

save(gss,file="Data/gss7221_r1_reltrad.RData")

