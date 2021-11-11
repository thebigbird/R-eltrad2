#Create a Figure for GSS attendance over time
#Plot the proportions over time
library(srvyr)
library(tidyverse)
library(survey)
library(ggthemes)
#
load("Data/gss7221_r1_reltrad.RData") #See my github for the code to make these data

#Turn things into factors and ager categories
gss = gss %>% 
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(birthyr = year - age) %>%
  mutate(age_cat = cut(birthyr, c(0,1928,1945,1965,1981,1997,2021))) %>% 
  mutate(partyid = as.factor(partyid),
         trump = as.factor(ifelse(pres16==2,1,0))) %>% 
         mutate(year=as.factor(year))

levels(gss$age_cat) = c("Great","Silent","Boomer","GenX","Millenial","GenZ")

#Calculate weighted proportions
#Create survey object using svyr
gss_svy18 <- gss %>% filter(!is.na(wtssall)) %>% 
  as_survey_design(weight = wtssall,
                   variables = c(year, reltrad, age_cat,partyid,race,trump))


gss_21 = svydesign(id = ~vpsu,
                   strata = ~vstrat,
                   nest=T,
                   weight = ~wtssps,
                   variables = ~reltrad+year+partyid+race,
                   data = filter(gss,year==2021))

gss_svy21 = as_survey_design(gss_21)

#Now calculate proportions
out = gss_svy18 %>% 
  drop_na(year, reltrad) %>% 
  group_by(year, reltrad) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T))

out2 = gss_svy21 %>% 
  drop_na(year, reltrad) %>% 
  select(year, reltrad) %>% 
  group_by(year,reltrad) %>%
  summarize(prop = survey_mean(na.rm=T, proportion = T))

out = bind_rows(out,out2)


out[out$year==2018,]
#Plot the affiliation over time
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

plotout = ggplot(data = out, aes(x=year, y=prop, color = reltrad, group = reltrad)) +
  geom_ribbon(aes(ymin = prop - 1.96*prop_se, ymax = prop + 1.96*prop_se),
              color = "white",alpha = .2, fill = "grey") +
  geom_point() +
  geom_line() +
  ylab("Proportion of US Population Identifying As...") +
  xlab("") +
  labs(title = "Religious Affiliation in the United States",
       subtitle = "General Social Survey",
       caption = "David Eagle @davideaglephd / Data: https://gss.norc.org/",
       color = "Religious Tradition")+
  theme_minimal() + scFill +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ggtitle("Religious Affiliation in the United States 1972-2021")

plotout

ggsave("Output/reltradGSS.pdf", width = 12, height=5, dpi="retina")


