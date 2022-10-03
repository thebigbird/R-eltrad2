#For easier recoding
#Get rid of the black oversamples - these throw off the proportions
#This takes the 72-18 GSS and recodes it to create reltrad. 
#Small changes made Nov. 8, 2021.
#PLEASE UPDATE YOUR CODE

#CONTAINS LIFEWAY CODE CORRECTION
#http://lifewayresearch.com/wp-content/uploads/2016/02/Stata_coding_reltrad_2_19_2016.pdf

#Created by David Eagle www.davideagle.org
#"code samples/GGplotReltrad7218.R" makes a picture of religious tradition over time

#Reltrad Function
#attendCor = T; filter non-denoms by monthly+ attendance
#nondenom = T; create a version of reltrad with nondenoms as a separate category

reltrad = function(gss,attendCor=F,nondenom=F){
  library(tidyverse)
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
  # 13) Inter-nondenominational
  # 98) Don't know
  # 99) No answer
  
  gss$reltrad=NA
  
  #Takes the GSS variable "RELIG" and recodes into 5 major categories
  gss = gss %>% mutate(xaffil = case_when(
    relig==1 ~ "prot",
    relig==2 ~ "cath",
    relig==3 ~ "jew",
    relig==4 ~ "nonaf",
    relig>=5&relig<=10 ~ "other",
    relig==11 ~ "prot",
    relig==12 ~ "other",
    relig==13 ~ "prot"
  )) %>% 
    mutate(xaffil = as.factor(xaffil))
  
  # The following code breaks down religious groups by evangelicals, black
  # Protestants, mainline, liberal and conservative nontraditional,
  # and Protestant nondenomination/no denomination.
  
  #####Black Protestants
  #####
  
  #Create a racial indicator using the GSS "race" variable in 1972-2018, constructed for 2021
  #Here white is actually non-black. Not MY choice to name this variable, that was in original
  gss = gss %>% mutate(black = ifelse(race==2,1,0),
                       white = ifelse(race==1|race==3,1,0))
  
  #Take the "other" Protestant denominations and pull out the 
  #historical Black denominations, e.g. COGIC
  #This uses the GSS variable OTHER, which is NORC-created from denom verbatims
  bpother = c(7, 14, 15, 21, 37, 38, 56, 78, 79, 85, 86, 
              87, 88, 98, 103, 104, 128, 133)
  #National baptists and AME, AMEZ
  bpdenom = c(12, 13, 20, 21)
  #Blacks in certain denoms get recoded as Black Protestant
  #Other baptist, amer. baptist, south. bap, other Methodists
  # Create indicator for black race in denom and other
  #When black in these denoms, call these Black Protestant denoms
  #93 other is missionary baptist
  bldenoms = c(23, 28, 18, 15, 10, 11, 14)
  
  gss = gss %>% mutate(bldenom = denom * black,
                       blother = other==93*black)
  
  gss = gss %>% mutate(xbp = case_when(
    other %in% bpother ~ 1,
    denom %in% bpdenom ~ 1,
    bldenom %in% bldenoms ~1,
    blother ~ 1,
    TRUE ~ 0
  ))
  
  #Evangelical Protestants#
  #Recode the evangelicals in the other variable
  gss$xev=gss$other
  evother=c(2, 3, 5, 6, 9, 10, 12, 13, 16, 18, 20, 22, 24, 26, 27, 28, 31, 32,
            34, 35, 36, 39, 41, 42, 43, 45, 47,51, 52, 53, 55, 57, 63, 65, 66, 
            67, 68, 69, 76, 77, 83, 84, 90, 91, 92, 94, 97, 100, 101, 102, 106,
            107, 108, 109, 110, 111, 112, 115, 116, 117, 118, 120, 121, 122, 124, 
            125, 127, 129, 131, 132, 134, 135, 138, 139, 140, 146)
  #Cons Lutherans, cons presbyterians denoms
  evdenom1 =c(32,33,34,42)
  #White folk in these denoms
  #White baptists, white other methodists
  evdenom2 = c(10,18,15,23,14)
  
  gss = gss %>% mutate(denomWh = denom * white,
                       otherWh = other * white) %>% 
    mutate(xev = case_when(other %in% evother ~ 1,
                           denom %in% evdenom1 ~ 1,
                           denomWh %in% evdenom2 ~ 1,
                           otherWh==93 ~ 1,   #Missionary Baptist
                           TRUE ~ 0)) %>% 
    mutate(xev = ifelse(xbp==1,0,xev))   #Capturing a couple cases where they are also classified as BP
  
  
  # Mainline Protestants
  #Mainline denoms in other and denom variables
  mpother=c(1,8,19,23,25,40, 44, 46, 48, 49, 50, 54, 70, 71, 72, 73, 81, 
            89, 96, 99, 105, 119, 148)
  mpdenom = c(30, 50, 35, 31, 38, 40, 48, 43, 22, 41)
  #Mainline baptist denom and methodists - if the R is white, they get coded mainline
  mpdenom2 = c(11, 28)
  
  gss = gss %>% mutate(xml= case_when(other %in% mpother ~ 1,
                                      denom %in% mpdenom ~ 1,
                                      denomWh %in% mpdenom2 ~ 1,
                                      TRUE ~ 0))
  
  #Catholics
  #Polish National Church and Catholic
  cathother = c(123,28)
  gss = gss %>% mutate(xcath = case_when(other %in% cathother ~ 1,
                                         xaffil=="cath" ~ 1,
                                         TRUE ~ 0))
  #Jews
  gss = gss %>% mutate(xjew = ifelse(xaffil=="jew",1,0))
  
  #Adherents of other religions.
  otherother = c(11, 17, 29, 30, 33, 58, 59, 60, 61, 62, 64, 74, 75, 80, 
                 82, 95, 113, 114, 130, 136, 141, 145)
  
  gss = gss %>% mutate(xother = case_when(other %in% otherother ~ 1,
                                          (xaffil=="other")&(xev==0) ~1,  #1 JW and 7 No denomination
                                          TRUE ~ 0
  ))
  
  #Unaffiliateds/Nonaffiliateds
  gss = gss %>% mutate(xnonaff = ifelse(xaffil=="nonaf",1,0))
  
  # #This recodes non-denoms based on their attendance 
  # #Non active Don't Know Protestants coded to nonaffil
  # Rest are either 1 = Protestant or 11 = Christian
  
  # DENOM==70 is "No denomination or non-denominational church", gotta check Protestant==1 to get to denom
  gss$xprotdk = ifelse(gss$denom == 70,1,0)
  gss$protND = gss$xprotdk
  # #Protestants who attend monthly or less get turned to NA
  if (attendCor==T) {gss$xprotdk[gss$xprotdk == 1 & gss$attend < 4] = 0}
  # #Active Don't Know Protestants coded to evangelicals who attend 
  gss$xev[gss$xprotdk == 1] = 1
  #
  #############################################
  #Lifeway correction
  #*This takes people who responded that they were Christian in the relig variable but didn't get asked the
  #followup and puts them into reltrad*
  #http://ryanburge.net/wp-content/uploads/2015/12/reltradarticle-1.pdf
  
  #Where people said they are Christian, but didn't answer the denomination question or gave "other" to
  #the denomination question. Really just prot don't know
  gss = gss %>%  mutate(xtn = ifelse(((denom==60|is.na(denom))&(relig==11)),1,0)) #16 people
  #Only weekly or +weekly attenders - this is a questionable assumption
  #Probably could be challenged.
  gss$protND[gss$xtn==1]=1
  if (attendCor==T) {gss$xtn[gss$attend < 4|is.na(gss$attend)] <- 0}
  gss$xev[gss$xtn ==1] <- 1
  #This takes people who responded that they were Interdenominational in the relig variable
  gss$inter <- gss$relig
  gss$inter <- car::recode(gss$inter, "13=1; else=0")
  gss$inter2 = gss$inter
  if (attendCor==T) {gss$inter[gss$attend < 4|is.na(gss$attend)] <- 0}
  gss$xev[gss$inter ==1] <- 1
  
  #############################################
  gss$reltrad = factor(NA, levels=c("Conservative Protestant",
                                    "Mainline Protestant",
                                    "Black Protestant",
                                    "Roman Catholic",
                                    "Jewish",
                                    "Other",
                                    "None"))
  
  
  gss$reltrad2 = factor(NA, levels=c("Conservative Protestant",
                                     "Mainline Protestant",
                                     "Black Protestant",
                                     "Roman Catholic",
                                     "Protestant No Denomination",
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
  
  gss$reltrad2[gss$xev==1]="Conservative Protestant"
  gss$reltrad2[gss$xml==1]="Mainline Protestant"
  gss$reltrad2[gss$xbp==1]="Black Protestant"
  gss$reltrad2[gss$xcath==1]="Roman Catholic"
  gss$reltrad2[gss$xjew==1]="Jewish"
  gss$reltrad2[gss$xother==1]="Other"
  gss$reltrad2[gss$xnonaff==1]="None"
  gss$reltrad2[gss$inter2==1]="Protestant No Denomination"
  gss$reltrad2[gss$protND==1]="Protestant No Denomination"
  
  if (nondenom==F) {return(gss$reltrad)} else {return(gss$reltrad2)}
  
}



