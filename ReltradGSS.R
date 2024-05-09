#Using the reltrad function:
#For easier recoding
library(car)
library(gssr)
library("tidyverse")
library(srvyr)
#data(gss_all)
#gss=haven::read_dta("~/Downloads/GSS_stata/gss7222_r2.dta")
#save(gss,file="gss.RData")
load("gss.RData")
#Start by getting functions
#
source("reltrad2Fn.R")
source("reltradFn.R")

#Get rid of the Black oversample
gss = gss %>% 
  select(year,age,wtssps,sample,race,denom,relig,attend,other, wtssall,partyid, homosex,attend) %>% 
  filter(sample!=4,sample!=5,sample!=7) %>% 
  mutate(wtssall = ifelse(is.na(wtssall), wtssps, wtssall))

#Cut by generation
gss = gss %>% 
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(birthyr = year - age) %>%
  mutate(age_cat = cut(birthyr, c(0,1928,1945,1965,1981,1997,2020))) %>% 
  mutate(reltrad = reltrad(gss),
         reltrad2 = reltrad2(gss))

levels(gss$age_cat) = c("Great","Silent","Boomer","GenX","Millenial","GenZ")

#Create party variable
gss = gss %>% 
  mutate(party = case_when(partyid < 3 ~ "Democrat",
                           partyid == 3 ~ "Independent",
                           partyid > 3 & partyid <6 ~ "Republican",
                           TRUE ~ NA_character_)) %>% 
  mutate(none=ifelse(reltrad2=="None","Yes","No"))

#Homosexual morality variable - recode to 1=not wrong
gss = gss %>% 
  mutate(homosexright = case_when(homosex==5 ~ NA_integer_,
                                 homosex==4 ~ 1,
                                 is.na(homosex) ~ NA_integer_,
                                 TRUE ~ 0))


#Calculate weighted proportions
#Create survey object using svyr
gss_svy <- gss %>% as_survey_design(1, weight = wtssall,
                                    variables = c(year, reltrad, reltrad2,age_cat, party,none,homosexright,attend))
#Now calculate proportions
out = gss_svy %>% 
  group_by(year, reltrad) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T)) %>%
  drop_na() %>% 
  ungroup()

out2 = gss_svy %>% 
  group_by(year, reltrad2) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T)) %>%
  drop_na() %>% 
  ungroup()

out4 = gss_svy %>% 
  group_by(year, party,reltrad2) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T)) %>%
  drop_na() %>% 
  ungroup()

outnone = gss_svy %>% 
  group_by(year, party,none) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T)) %>%
  drop_na() %>% 
  ungroup()

outhomor = gss_svy %>% 
  group_by(year, reltrad2,homosexright) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T)) %>%
  drop_na() %>% 
  ungroup()

View(outhomor)


#A nice set o' colors
scFill = scale_color_manual(values = 
                              c("#1B9E77", 
                                "#DDC849",
                                "#7570b3", 
                                "#a6761d",
                                "#e7298a",
                                "#1f78b4",
                                "#a9a9a9",
                                "#cc99ff"))



outa <- out %>% filter(reltrad==c("Conservative Protestant","Mainline Protestant","Black Protestant"))
outb <- out2 %>% filter(reltrad2==c("Conservative Protestant","Mainline Protestant","Black Protestant"))


plotout1 = ggplot(data = outa, 
                  aes(x=year, 
                      y=prop,
                      color = reltrad,
                      group = reltrad)) +
  geom_point(alpha=.2) +
  geom_line(alpha=.2) +
  geom_point(data=outb,aes(x=year,y=prop, color = reltrad2, group=reltrad2)) +
  geom_line(data=outb,aes(x=year,y=prop, color = reltrad2, group=reltrad2)) +
  ylab("Proportion of US Population Identifying As...") +
  xlab("") +
  labs(title = "Religious Affiliation in the United States, a tale of two Reltrads",
       subtitle = "Reltrad2 (Dark), Reltrad O.G. (Light lines)",
       caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Religious Tradition")+
  theme_minimal() + scFill +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) 

#ggsave(plotout,"Output/reltradGSS.png", width = 12, height=5, dpi="retina")
 plotout1
 
 #Light lines are the old reltrad. Our correction is in dark.
table(out2$year)

plotout2 = ggplot(data = out2, 
                  aes(x=year, 
                      y=prop,
                      color = reltrad2,
                      group = reltrad2)) +
  geom_ribbon(aes(ymin = prop - 1.96*prop_se,
                  ymax = prop + 1.96*prop_se),
              color = "white",
              alpha = .2, fill = "grey") +
  geom_point() +
  geom_line() +
  ylab("Proportion of US Population Identifying As...") +
  xlab("") +
  labs(title = "Religious Affiliation in the United States",
       subtitle = "1972-2018, General Social Survey",
      # caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Religious Tradition")+
  theme_minimal() + scFill +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
plotout2

out5 = out4 %>% 
  filter(party=="Democrat"|party=="Republican",reltrad2!="Other"&reltrad2!="Jewish"&reltrad2!="Black Protestant")


plotout3 = ggplot(data = out5, 
                  aes(x=year, 
                      y=prop,
                      color = reltrad2,
                      group = reltrad2)) +
 # geom_ribbon(aes(ymin = prop - 1.96*prop_se,
  #                ymax = prop + 1.96*prop_se),
   #           color = "white",
    #          alpha = .2, fill = "grey") +
  geom_point() +
  geom_line() +
  ylab("Proportion of US Population Identifying As...") +
  xlab("") +
  labs(title = "Religious Affiliation in the United States",
       subtitle = "1972-2018, General Social Survey",
       # caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Religious Tradition")+
  theme_minimal() + scFill +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  facet_grid(~party)

plotout3

View(out4)
ggsave("~/Downloads/plot2.png", width = 6.5, height=3.7, dpi="retina")




plotout3 = ggplot(data = filter(outnone,none=="Yes"&party!="Independent"), 
                  aes(x=year, 
                      y=prop,
                      color = party,
                      group = party)) +
  # geom_ribbon(aes(ymin = prop - 1.96*prop_se,
  #                ymax = prop + 1.96*prop_se),
  #           color = "white",
  #          alpha = .2, fill = "grey") +
  geom_point() +
  geom_line() +
  ylab("Americans who identify as having no religion...") +
  xlab("") +
  labs(title = "Religious Nones by Party",
       subtitle = "1972-2018, General Social Survey",
       # caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Political Leaning")+
  theme_minimal()  + scale_color_manual(values=c("blue","red"))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
plotout3
ggsave("~/Downloads/plot3.png", width = 6.5, height=3.7, dpi="retina")

plotout4 = ggplot(data = filter(out4,reltrad2=="Conservative Protestant"&party!="Independent"), 
                  aes(x=year, 
                      y=prop,
                      color = party,
                      group = party)) +
  # geom_ribbon(aes(ymin = prop - 1.96*prop_se,
  #                ymax = prop + 1.96*prop_se),
  #           color = "white",
  #          alpha = .2, fill = "grey") +
  geom_point() +
  geom_line() +
  ylab("Americans who identify with a Conservative Protestant Denomination...") +
  xlab("") +
  labs(title = "Conservative Protestants by Party",
       subtitle = "1972-2018, General Social Survey",
       # caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Political Leaning")+
  theme_minimal()  + scale_color_manual(values=c("blue","red"))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
plotout4
ggsave("~/Downloads/plot5.png", width = 13, height=7.4, dpi="retina")



plotouth = ggplot(data = filter(outhomor,homosexright==1,year>2000), 
                  aes(x=year, 
                      y=prop,
                      color = reltrad2,
                      group = reltrad2)) +
  # geom_ribbon(aes(ymin = prop - 1.96*prop_se,
  #                ymax = prop + 1.96*prop_se),
  #           color = "white",
  #          alpha = .2, fill = "grey") +
  geom_point() +
  geom_line() +
  ylab("Homosexual relationships are 'not wrong at all'...") +
  xlab("") +
  labs(title = "Support for Homosexual Relationship by Religious Tradition",
       subtitle = "1972-2018, General Social Survey",
        caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Political Leaning")+
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
plotouth
ggsave("~/Downloads/plot6.png", width = 13, height=7.4, dpi="retina")

 View(outhomor)

