#Using the reltrad function:
#For easier recoding
library(car)
library(gssr)
library("tidyverse")
library(srvyr)
data(gss_all)
gss=gss_all;rm(gss_all)
getwd()

#Start by getting functions
#
source("reltrad2Fn.R")
source("reltradFn.R")

#Get rid of the Black oversample
gss = gss %>% 
  filter(sample!=4,sample!=5,sample!=7) %>% 
  filter(year<2021)

#Cut by generation
gss = gss %>% 
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(birthyr = year - age) %>%
  mutate(age_cat = cut(birthyr, c(0,1928,1945,1965,1981,1997,2020))) %>% 
  mutate(reltrad = reltrad(gss),
         reltrad2 = reltrad2(gss))

levels(gss$age_cat) = c("Great","Silent","Boomer","GenX","Millenial","GenZ")

#Calculate weighted proportions
#Create survey object using svyr
gss_svy <- gss %>% as_survey_design(1, weight = wtssall,
                                    variables = c(year, reltrad, reltrad2,age_cat))
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
  labs(title = "Religious Affiliation in the United States",
       subtitle = "General Social Survey",
       caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Religious Tradition")+
  theme_minimal() + scFill +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ggtitle("Religious Affiliation in the United States 1972-2018 - Reltrad2 (Dark), Reltrad O.G. (Light lines).")
#ggsave(plotout,"Output/reltradGSS.png", width = 12, height=5, dpi="retina")


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
       subtitle = "General Social Survey",
       caption = "David Eagle / Data: https://gss.norc.org/",
       color = "Religious Tradition")+
  theme_minimal() + scFill +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ggtitle("Religious Affiliation in the United States 1972-2018 - Reltrad2")

#ggsave(plotout,"Output/reltradGSS.png", width = 12, height=5, dpi="retina")


plotout1 #Light lines are the old reltrad. Our correction is in dark.
